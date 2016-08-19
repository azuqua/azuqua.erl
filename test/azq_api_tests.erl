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

sign_data_map(_S) ->
  Data = #{foo => bar},
  Verb = get,
  Path = <<"path">>,
  Time = <<"t">>,
  {ok, Act} = azq_api:sign_data(Data, Verb, Path, Time),
  Exp = azq_api_utils:sign_data(<<"secret">>, jiffy:encode(Data), Verb, Path, Time),
  ?_assertEqual(Act, Exp).

sign_data_list(_S) ->
  Data = [{foo, bar}],
  Verb = get,
  Path = <<"path">>,
  Time = <<"t">>,
  {ok, Act} = azq_api:sign_data(Data, Verb, Path, Time),
  Exp = azq_api_utils:sign_data(<<"secret">>, jiffy:encode({Data}), Verb, Path, Time),
  ?_assertEqual(Act, Exp).

sign_data_bin(_S) ->
  Data = jiffy:encode(#{foo => bar}),
  Verb = get,
  Path = <<"path">>,
  Time = <<"t">>,
  {ok, Act} = azq_api:sign_data(Data, Verb, Path, Time),
  Exp = azq_api_utils:sign_data(<<"secret">>, Data, Verb, Path, Time),
  ?_assertEqual(Act, Exp).

retry_error(_S) ->
  Res = azq_api:retry(bad, #{}),
  ?assertMatch({error, _}, Res),
  Res2 = azq_api:retry(<<"flo">>, bad),
  ?_assertMatch({error, _}, Res2).

async_retry_error(_S) ->
  Res = azq_api:async_retry(bad, #{}),
  ?assertMatch({error, _}, Res),
  Res2 = azq_api:async_retry(<<"flo">>, bad),
  ?_assertMatch({error, _}, Res2).

invoke_error(_S) ->
  Res = azq_api:invoke(bad, #{}),
  ?assertMatch({error, _}, Res),
  Res2 = azq_api:invoke(<<"flo">>, bad),
  ?_assertMatch({error, _}, Res2).

async_invoke_error(_S) ->
  Res = azq_api:async_invoke(bad, #{}),
  ?assertMatch({error, _}, Res),
  Res2 = azq_api:async_invoke(<<"flo">>, bad),
  ?_assertMatch({error, _}, Res2).

inputs_error(_S) ->
  Res = azq_api:inputs(bad),
  ?_assertMatch({error, _}, Res).

async_inputs_error(_S) ->
  Res = azq_api:async_inputs(bad),
  ?_assertMatch({error, _}, Res).

inject_error(_S) ->
  Res = azq_api:inject(bad, #{}),
  ?assertMatch({error, _}, Res),
  Res2 = azq_api:inject(<<"flo">>, bad),
  ?_assertMatch({error, _}, Res2).

async_inject_error(_S) ->
  Res = azq_api:async_inject(bad, #{}),
  ?assertMatch({error, _}, Res),
  Res2 = azq_api:async_inject(<<"flo">>, bad),
  ?_assertMatch({error, _}, Res2).

schedule_error(_S) ->
  Res = azq_api:schedule(bad, #{}),
  ?assertMatch({error, _}, Res),
  Res2 = azq_api:schedule(<<"flo">>, bad),
  ?_assertMatch({error, _}, Res2).

async_schedule_error(_S) ->
  Res = azq_api:async_schedule(bad, #{}),
  ?assertMatch({error, _}, Res),
  Res2 = azq_api:async_schedule(<<"flo">>, bad),
  ?_assertMatch({error, _}, Res2).
