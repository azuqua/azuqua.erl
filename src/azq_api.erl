-module(azq_api).

-behavior(gen_server).

-export([init/1, 
         terminate/2, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         code_change/3]).
-export([new/2,
         new/3,
         sign_data/4,
         get_flos/0,
         async_get_flos/0,
         yield_get_flos/1,
         retry/2,
         async_retry/2,
         yield_retry/1,
         invoke/2,
         async_invoke/2,
         yield_invoke/1,
         inputs/1,
         async_inputs/1,
         yield_inputs/1,
         inject/2,
         async_inject/2,
         yield_inject/1,
         schedule/2,
         async_schedule/2,
         yield_schedule/1]).

-record(api_state, {key = <<>>, secret = <<>>, opts = #{}, promises=#{},
                    flos = #{}}).
-record(promise, {return, ref, data, client}).
-record(request, {path = <<>>, qs = <<>>, body = <<>>, headers = [],
                  method = get}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% INTERFACE FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(Key, Secret) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Key, Secret]).

new(Key, Secret, Opts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Key, Secret, Opts]).

sign_data(Data = #{}, Verb, Path, Time) ->
  BData = jiffy:encode(Data),
  sign_data(BData, Verb, Path, Time);
sign_data(Data, Verb, Path, Time) when is_list(Data) ->
  BData = jiffy:encode({Data}),
  sign_data(BData, Verb, Path, Time);
sign_data(Data, _, _, _) when not is_binary(Data) ->
  {error, ebadsign};
sign_data(_, Verb, _, _) when not is_binary(Verb) ->
  {error, ebadverb};
sign_data(_, _, Path, _) when not is_binary(Path) ->
  {error, ebadpath};
sign_data(_, _, _, Time) when not is_binary(Time) ->
  {error, ebadtime};
sign_data(Data, Verb, Path, Time) ->
  gen_server:call(?MODULE, {sign_data, Data, Verb, Path, Time}).

get_flos() ->
  Res = gen_server:call(?MODULE, get_flos),
  parse_yield(get_flos, Res).

async_get_flos() ->
  gen_server:call(?MODULE, async_get_flos).

yield_get_flos(Ref) ->
  Res = gen_server:call(?MODULE, {yield, Ref}),
  parse_yield(get_flos, Res).

retry(Flo, _Data) when not is_binary(Flo) ->
  {error, ebadflo};
retry(Flo, Data) when is_list(Data) ->
  retry(Flo, jiffy:encode({Data}));
retry(Flo, Data) when is_map(Data) ->
  retry(Flo, jiffy:encode(Data));
retry(_, Data) when not is_binary(Data) ->
  {error, ebadretry};
retry(Flo, Data) ->
  Res = gen_server:call(?MODULE, {retry, Flo, Data}),
  parse_yield(flo, Res).

async_retry(Flo, _Data) when not is_binary(Flo) ->
  {error, ebadflo};
async_retry(Flo, Data) when is_list(Data) ->
  async_retry(Flo, jiffy:encode({Data}));
async_retry(Flo, Data) when is_map(Data) ->
  async_retry(Flo, jiffy:encode(Data));
async_retry(_, Data) when not is_binary(Data) ->
  {error, ebadretry};
async_retry(Flo, Data) ->
  gen_server:call(?MODULE, {async_retry, Flo, Data}).

yield_retry(Ref) ->
  Res = gen_server:call(?MODULE, {yield, Ref}),
  parse_yield(flo, Res).

invoke(Flo, _Data) when not is_binary(Flo) ->
  {error, ebadflo};
invoke(Flo, Data) when is_list(Data) ->
  invoke(Flo, jiffy:encode({Data}));
invoke(Flo, Data) when is_map(Data) ->
  invoke(Flo, jiffy:encode(Data));
invoke(_, Data) when not is_binary(Data) ->
  {error, ebadinvoke};
invoke(Flo, Data) ->
  Res = gen_server:call(?MODULE, {invoke, Flo, Data}),
  parse_yield(flo, Res).

async_invoke(Flo, _Data) when not is_binary(Flo) ->
  {error, ebadflo};
async_invoke(Flo, Data) when is_list(Data) ->
  async_invoke(Flo, jiffy:encode({Data}));
async_invoke(Flo, Data) when is_map(Data) ->
  async_invoke(Flo, jiffy:encode(Data));
async_invoke(_, Data) when not is_binary(Data) ->
  {error, ebadinvoke};
async_invoke(Flo, Data) ->
  gen_server:call(?MODULE, {async_invoke, Flo, Data}).

yield_invoke(Ref) ->
  Res = gen_server:call(?MODULE, {yield, Ref}),
  parse_yield(flo, Res).

inputs(Flo) when not is_binary(Flo) ->
  {error, ebadflo};
inputs(Flo) ->
  Res = gen_server:call(?MODULE, {inputs, Flo}),
  parse_yield(inputs, Res).

async_inputs(Flo) when not is_binary(Flo) ->
  {error, ebadflo};
async_inputs(Flo) ->
  gen_server:call(?MODULE, {async_inputs, Flo}).

yield_inputs(Ref) ->
  Res = gen_server:call(?MODULE, {yield, Ref}),
  parse_yield(inputs, Res).

inject(Flo, _Data) when not is_binary(Flo) ->
  {error, ebadflo};
inject(Flo, Data) when is_list(Data) ->
  inject(Flo, jiffy:encode({Data}));
inject(Flo, Data) when is_map(Data) ->
  inject(Flo, jiffy:encode(Data));
inject(_, Data) when not is_binary(Data) ->
  {error, ebadinject};
inject(Flo, Data) ->
  Res = gen_server:call(?MODULE, {inject, Flo, Data}),
  parse_yield(flo, Res).

async_inject(Flo, _Data) when not is_binary(Flo) ->
  {error, ebadflo};
async_inject(Flo, Data) when is_list(Data) ->
  async_inject(Flo, jiffy:encode({Data}));
async_inject(Flo, Data) when is_map(Data) ->
  async_inject(Flo, jiffy:encode(Data));
async_inject(_, Data) when not is_binary(Data) ->
  {error, ebadinject};
async_inject(Flo, Data) ->
  gen_server:call(?MODULE, {async_inject, Flo, Data}).

yield_inject(Ref) ->
  Res = gen_server:call(?MODULE, {yield, Ref}),
  parse_yield(flo, Res).

schedule(Flo, _Data) when not is_binary(Flo) ->
  {error, ebadflo};
schedule(Flo, Data) when is_list(Data) ->
  schedule(Flo, jiffy:encode({Data}));
schedule(Flo, Data) when is_map(Data) ->
  schedule(Flo, jiffy:encode(Data));
schedule(_, Data) when not is_binary(Data) ->
  {error, ebadschedule};
schedule(Flo, Data) ->
  Res = gen_server:call(?MODULE, {schedule, Flo, Data}),
  parse_yield(schedule, Res).

async_schedule(Flo, _Data) when not is_binary(Flo) ->
  {error, ebadflo};
async_schedule(Flo, Data) when is_list(Data) ->
  async_schedule(Flo, jiffy:encode({Data}));
async_schedule(Flo, Data) when is_map(Data) ->
  async_schedule(Flo, jiffy:encode(Data));
async_schedule(_, Data) when not is_binary(Data) ->
  {error, ebadschedule};
async_schedule(Flo, Data) ->
  gen_server:call(?MODULE, {async_schedule, Flo, Data}).

yield_schedule(Ref) ->
  Res = gen_server:call(?MODULE, {yield, Ref}),
  parse_yield(schedule, Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GEN_SERVER CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Key, Secret]) ->
  HeaderOpts = #{<<"Content-Type">> => <<"application/json">>},
  Opts = #{host => <<"api.azuqua.com">>,
           port => 443,
           protocol => <<"https">>,
           headers => HeaderOpts},
  init([Key, Secret, Opts]);
init([Key, Secret, Opts]) when is_list(Key) ->
  init([erlang:list_to_binary(Key), Secret, Opts]);
init([<<>>, _, _]) ->
  {stop, enokey};
init([Key, _, _]) when not is_binary(Key) ->
  {stop, ebadkey};
init([Key, Secret, Opts]) when is_list(Secret) ->
  init([Key, erlang:list_to_binary(Secret), Opts]);
init([_, <<>>, _]) ->
  {stop, enosecret};
init([_, Secret, _]) when not is_binary(Secret) ->
  {stop, ebadsecret};
init([_, _, Opts]) when not is_map(Opts) ->
  {stop, ebadopts};
init([Key, Secret, Opts]) ->
  State = #api_state{key=Key, secret=Secret, opts=Opts},
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

handle_call({sign_data, Data, Verb, Path, Time}, _From, State) ->
  Secret = State#api_state.secret,
  NData = azq_api_utils:sign_data(Secret, Data, Verb, Path, Time),
  {reply, NData, State};
handle_call(async_get_flos, _From, State) ->
  Ref = erlang:make_ref(),
  NState = promise_get_flos(Ref, State),
  {reply, Ref, NState};
handle_call(get_flos, From, State) ->
  Ref = erlang:make_ref(),
  NState = promise_get_flos(Ref, State),
  Execs = NState#api_state.promises,
  NExecs = attach_promise(Ref, From, Execs),
  {noreply, NState#api_state{promises=NExecs}};
handle_call({async_retry, Flo, Data}, _From, State) ->
  Ref = erlang:make_ref(),
  NState = promise_retry(Ref, Flo, Data, State),
  {reply, Ref, NState};
handle_call({retry, Flo, Data}, From, State) ->
  Ref = erlang:make_ref(),
  NState = promise_retry(Ref, Flo, Data, State),
  Execs = NState#api_state.promises,
  NExecs = attach_promise(Ref, From, Execs),
  {noreply, NState#api_state{promises=NExecs}};
handle_call({async_invoke, Flo, Data}, _From, State) ->
  Ref = erlang:make_ref(),
  NState = promise_invoke(Ref, Flo, Data, State),
  {reply, Ref, NState};
handle_call({invoke, Flo, Data}, From, State) ->
  Ref = erlang:make_ref(),
  NState = promise_invoke(Ref, Flo, Data, State),
  Execs = NState#api_state.promises,
  NExecs = attach_promise(Ref, From, Execs),
  {noreply, NState#api_state{promises=NExecs}};
handle_call({async_inputs, Flo}, _From, State) ->
  Ref = erlang:make_ref(),
  NState = promise_inputs(Ref, Flo, State),
  {reply, Ref, NState};
handle_call({inputs, Flo}, From, State) ->
  Ref = erlang:make_ref(),
  NState = promise_inputs(Ref, Flo, State),
  Execs = NState#api_state.promises,
  NExecs = attach_promise(Ref, From, Execs),
  {noreply, NState#api_state{promises=NExecs}};
handle_call({async_inject, Flo, Data}, _From, State) ->
  Ref = erlang:make_ref(),
  NState = promise_inject(Ref, Flo, Data, State),
  {reply, Ref, NState};
handle_call({inputs, Flo, Data}, From, State) ->
  Ref = erlang:make_ref(),
  NState = promise_inject(Ref, Flo, Data, State),
  Execs = NState#api_state.promises,
  NExecs = attach_promise(Ref, From, Execs),
  {noreply, NState#api_state{promises=NExecs}};
handle_call({async_schedule, Flo, Data}, _From, State) ->
  Ref = erlang:make_ref(),
  NState = promise_schedule(Ref, Flo, Data, State),
  {reply, Ref, NState};
handle_call({schedule, Flo, Data}, From, State) ->
  Ref = erlang:make_ref(),
  NState = promise_schedule(Ref, Flo, Data, State),
  Execs = NState#api_state.promises,
  NExecs = attach_promise(Ref, From, Execs),
  {noreply, NState#api_state{promises=NExecs}}.

handle_cast(_Ref, _State) ->
  ok.

handle_info(_Info, _State) ->
  ok.

code_change(_OldVsn, _State, _Extra) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

promise_get_flos(Ref, State) ->
  Request = #request{path = <<"/account/flos">>, method = get},
  {ok, CRef} = async_request(Request, State),
  create_promise(Ref, CRef, State).

promise_retry(Ref, Flo, Data, State) ->
  Path = << <<"/flo/">>/binary, Flo/binary, <<"/retry">>/binary >>,
  Request = #request{path = Path, body = Data, method = post},
  {ok, CRef} = async_request(Request, State),
  create_promise(Ref, CRef, State).

promise_invoke(Ref, Flo, Data, State) ->
  Path = << <<"/flo/">>/binary, Flo/binary, <<"/invoke">>/binary >>,
  Request = #request{path = Path, body = Data, method = post},
  {ok, CRef} = async_request(Request, State),
  create_promise(Ref, CRef, State).

promise_inputs(Ref, Flo, State) ->
  Path = << <<"/flo/">>/binary, Flo/binary, <<"/inputs">>/binary >>,
  Request = #request{path = Path, method = get},
  {ok, CRef} = async_request(Request, State),
  create_promise(Ref, CRef, State).

promise_inject(Ref, Flo, Data, State) ->
  Path = << <<"/flo/">>/binary, Flo/binary, <<"/inject">>/binary >>,
  Request = #request{path = Path, body = Data, method = post},
  {ok, CRef} = async_request(Request, State),
  create_promise(Ref, CRef, State).

promise_schedule(Ref, Flo, Data, State) ->
  Path = << <<"/flo/">>/binary, Flo/binary, <<"/schedule">>/binary >>,
  Request = #request{path = Path, body = Data, method = post},
  {ok, CRef} = async_request(Request, State),
  create_promise(Ref, CRef, State).

attach_promise(Ref, From, Execs) ->
  RefExec = maps:get(Ref, Execs),
  maps:put(Ref, RefExec#promise{return=From}, Execs).

create_promise(Ref, CRef, State) ->
  Execs = State#api_state.promises,
  NExecs = maps:put(Ref, #promise{ref=Ref, client=CRef}, Execs),
  State#api_state{promises=NExecs}.

async_request(#request{method=Method, qs=QS, headers=Headers, body=Body,
                       path=Path}, State) ->
  URL = azq_api_utils:construct_url(State#api_state.opts, Path, QS),
  NHeaders = azq_api_utils:construct_headers(State#api_state.opts, Headers),
  Opts = [async, {connect_timeout}, {recv_timeout}],
  hackney:request(Method, URL, NHeaders, Body, Opts).

parse_yield(get_flos, Res) ->
  Parsed = jiffy:decode(Res, [return_maps]),
  lists:map(Parsed, fun parse_get_flos_return/1);
parse_yield(flo, Res) ->
  jiffy:decode(Res, [return_maps]);
parse_yield(inputs, Res) ->
  jiffy:decode(Res, [return_maps]);
parse_yield(schedule, Res) ->
  jiffy:decode(Res, [return_maps]).

parse_get_flos_return(Flo) ->
  Select = [<<"id">>,
            <<"alias">>,
            <<"name">>,
            <<"description">>,
            <<"org_id">>,
            <<"active">>,
            <<"published">>],
  maps:with(Flo, Select).