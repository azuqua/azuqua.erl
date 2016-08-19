-module(azq_api_tests).

-include("../src/azq_api.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(API_KEY, <<"d9da0ea5efb58b22545f909e7754235bb9e7fad5">>).
-define(API_SECRET, <<"5686f7797cd31e366608b08fb9460a9926facacd876bb5f70cf872083a34f2cb">>).

sign_data_test_() ->
  {"sign_data unit test",
   {setup,
    fun init_default_api/0,
    fun kill_default_api/1,
    fun (S) ->
        [sign_data_map(S),
         sign_data_list(S),
         sign_data_bin(S)]
    end
   }
  }.

retry_test_() ->
  {"get_flos unit test",
   {setup,
    fun init_test_api/0,
    fun kill_test_api/1,
    fun (S) ->
        [retry_error(S), async_retry_error(S)]
    end
   }
  }.

invoke_test_() ->
  {"get_flos unit test",
   {setup,
    fun init_test_api/0,
    fun kill_test_api/1,
    fun (S) ->
        [invoke_error(S), async_invoke_error(S)]
    end
   }
  }.

inputs_test_() ->
  {"get_flos unit test",
   {setup,
    fun init_test_api/0,
    fun kill_test_api/1,
    fun (S) ->
        [inputs_error(S), async_inputs_error(S)]
    end
   }
  }.

inject_test_() ->
  {"get_flos unit test",
   {setup,
    fun init_test_api/0,
    fun kill_test_api/1,
    fun (S) ->
        [inject_error(S), async_inject_error(S)]
    end
   }
  }.

schedule_test_() ->
  {"get_flos unit test",
   {setup,
    fun init_test_api/0,
    fun kill_test_api/1,
    fun (S) ->
        [schedule_error(S), async_schedule_error(S)]
    end
   }
  }.

promise_test_() ->
  [create_promise(), attach_promise(), remove_promise()].

parse_yield_test_() ->
  [parse_yield_get_flos(),
   parse_yield_flo(),
   parse_yield_inputs(),
   parse_yield_schedule(),
   parse_yield_errors()].

%%%%%%%%%%%%%%%%%%%%%%
%%% INIT FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%

init_default_api() ->
  {ok, Pid} = azq_api:new(<<"key">>, <<"secret">>),
  Pid.

kill_default_api(S) ->
  gen_server:stop(S).

init_test_api() ->
  {ok, Pid} = azq_api:new(?API_KEY, ?API_SECRET),
  Pid.

kill_test_api(S) ->
  gen_server:stop(S).

%%%%%%%%%%%%%%%%%%%%%%
%%% TEST FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%

sign_data_map(S) ->
  Data = #{foo => bar},
  Verb = get,
  Path = <<"path">>,
  Time = <<"t">>,
  {ok, Act} = azq_api:sign_data(S, Data, Verb, Path, Time),
  Exp = azq_api_utils:sign_data(<<"secret">>, jiffy:encode(Data), Verb, Path, Time),
  ?_assertEqual(Act, Exp).

sign_data_list(S) ->
  Data = [{foo, bar}],
  Verb = get,
  Path = <<"path">>,
  Time = <<"t">>,
  {ok, Act} = azq_api:sign_data(S, Data, Verb, Path, Time),
  Exp = azq_api_utils:sign_data(<<"secret">>, jiffy:encode({Data}), Verb, Path, Time),
  ?_assertEqual(Act, Exp).

sign_data_bin(S) ->
  Data = jiffy:encode(#{foo => bar}),
  Verb = get,
  Path = <<"path">>,
  Time = <<"t">>,
  {ok, Act} = azq_api:sign_data(S, Data, Verb, Path, Time),
  Exp = azq_api_utils:sign_data(<<"secret">>, Data, Verb, Path, Time),
  ?_assertEqual(Act, Exp).

retry_error(S) ->
  Res = azq_api:retry(S, bad, #{}),
  ?assertMatch({error, _}, Res),
  Res2 = azq_api:retry(S, <<"flo">>, bad),
  ?_assertMatch({error, _}, Res2).

async_retry_error(S) ->
  Res = azq_api:async_retry(S, bad, #{}),
  ?assertMatch({error, _}, Res),
  Res2 = azq_api:async_retry(S, <<"flo">>, bad),
  ?_assertMatch({error, _}, Res2).

invoke_error(S) ->
  Res = azq_api:invoke(S, bad, #{}),
  ?assertMatch({error, _}, Res),
  Res2 = azq_api:invoke(S, <<"flo">>, bad),
  ?_assertMatch({error, _}, Res2).

async_invoke_error(S) ->
  Res = azq_api:async_invoke(S, bad, #{}),
  ?assertMatch({error, _}, Res),
  Res2 = azq_api:async_invoke(S, <<"flo">>, bad),
  ?_assertMatch({error, _}, Res2).

inputs_error(S) ->
  Res = azq_api:inputs(S, bad),
  ?_assertMatch({error, _}, Res).

async_inputs_error(S) ->
  Res = azq_api:async_inputs(S, bad),
  ?_assertMatch({error, _}, Res).

inject_error(S) ->
  Res = azq_api:inject(S, bad, #{}),
  ?assertMatch({error, _}, Res),
  Res2 = azq_api:inject(S, <<"flo">>, bad),
  ?_assertMatch({error, _}, Res2).

async_inject_error(S) ->
  Res = azq_api:async_inject(S, bad, #{}),
  ?assertMatch({error, _}, Res),
  Res2 = azq_api:async_inject(S, <<"flo">>, bad),
  ?_assertMatch({error, _}, Res2).

schedule_error(S) ->
  Res = azq_api:schedule(S, bad, #{}),
  ?assertMatch({error, _}, Res),
  Res2 = azq_api:schedule(S, <<"flo">>, bad),
  ?_assertMatch({error, _}, Res2).

async_schedule_error(S) ->
  Res = azq_api:async_schedule(S, bad, #{}),
  ?assertMatch({error, _}, Res),
  Res2 = azq_api:async_schedule(S, <<"flo">>, bad),
  ?_assertMatch({error, _}, Res2).

create_promise() ->
  Ref = erlang:make_ref(),
  State = #api_state{},
  NState = azq_api:create_promise(Ref, State),
  P = maps:get(Ref, NState#api_state.promises),
  ?_assertMatch(#promise{cref=Ref, state=init}, P).

attach_promise() ->
  Ref = erlang:make_ref(),
  From = erlang:make_ref(),
  State = #api_state{},
  State1 = azq_api:create_promise(Ref, State),
  Execs = azq_api:attach_promise(Ref, From, State1#api_state.promises),
  P = maps:get(Ref, Execs),
  ?_assertMatch(#promise{cref=Ref, return=From, state=init}, P).

remove_promise() ->
  Ref = erlang:make_ref(),
  State = #api_state{},
  State1 = azq_api:create_promise(Ref, State),
  State2 = azq_api:remove_promise(Ref, State1),
  P = maps:get(Ref, State2#api_state.promises, undefined),
  ?_assertEqual(P, undefined).

parse_yield_get_flos() ->
  Out = [#{<<"id">> => <<"1">>}, #{<<"id">> => <<"2">>}],
  Res = jiffy:encode(Out),
  {ok, F} = azq_api:parse_yield(get_flos, {ok, Res}),
  ?assertEqual(F, Out),

  Out1 = [#{<<"foo">> => <<"bar">>}, #{<<"id">> => <<"2">>}],
  Res1 = jiffy:encode(Out1),
  {ok, F1} = azq_api:parse_yield(get_flos, {ok, Res1}),
  ?_assertEqual(F1, [#{}, #{<<"id">> => <<"2">>}]).

parse_yield_flo() ->
  Out = #{<<"foo">> => <<"bar">>},
  Res = jiffy:encode(Out),
  {ok, F} = azq_api:parse_yield(flo, {ok, Res}),
  ?_assertEqual(F, Out).

parse_yield_inputs() ->
  Out = #{<<"foo">> => <<"bar">>},
  Res = jiffy:encode(Out),
  {ok, F} = azq_api:parse_yield(inputs, {ok, Res}),
  ?_assertEqual(F, Out).

parse_yield_schedule() ->
  Out = #{<<"foo">> => <<"bar">>},
  Res = jiffy:encode(Out),
  {ok, F} = azq_api:parse_yield(schedule, {ok, Res}),
  ?_assertEqual(F, Out).

parse_yield_errors() ->
  Out = #{<<"foo">> => <<"bar">>},
  Res = jiffy:encode(Out),
  {error, F} = azq_api:parse_yield(schedule, {error, Res}),
  ?_assertEqual(F, Out).
