-module(azq_api_SUITE).

-export([all/0,
         groups/0]).
-export([init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).
-export([get_flos_sync_test/1,
         get_flos_async_test/1,
         promise_get_flos_test/1]).

-include_lib("common_test/include/ct.hrl").
-include("../src/azq_api.hrl").

%%%%%%%%%%%%%%%%%%%%%%
%%% INIT FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%

all() ->
  [{group, internal}, {group, external}].

groups() ->
  [
   {
    internal,
    [],
    [promise_get_flos_test]
   },
   {
    external,
    [],
    [get_flos_sync_test,
     get_flos_async_test]
   }
  ].

init_per_suite(Config) ->
  hackney:start(),
  Key = erlang:list_to_binary(os:getenv("API_KEY")),
  Secret = erlang:list_to_binary(os:getenv("API_SECRET")),
  [{key, Key}, {secret, Secret} | Config].

end_per_suite(Config) ->
  hackney:stop(),
  Config.

init_per_group(internal, Config) ->
  BaseStr = os:getenv("API_BASE", <<"https://api.azuqua.com">>),
  Base = erlang:list_to_binary(BaseStr),
  Headers = [{<<"Content-Type">>, <<"application/json">>}],
  Opts = #{base => Base, headers => Headers},
  {ok, State} = azq_api:init([?config(key, Config), ?config(secret, Config), Opts]),
  [{state, State} | Config];
init_per_group(external, Config) ->
  Config.

end_per_group(internal, Config) ->
  Config;
end_per_group(external, Config) ->
  Config.

init_per_testcase(promise_get_flos_test, Config) ->
  Config;
init_per_testcase(_, Config) ->
  Key = ?config(key, Config),
  Secret = ?config(secret, Config),
  BaseStr = os:getenv("API_BASE", <<"https://api.azuqua.com">>),
  Base = erlang:list_to_binary(BaseStr),
  Headers = [{<<"Content-Type">>, <<"application/json">>}],
  {ok, Pid} = azq_api:new(Key, Secret, #{base => Base, headers => Headers}),
  [{api_pid, Pid} | Config].

end_per_testcase(promise_get_flos_test, Config) ->
  Config;
end_per_testcase(_, Config) ->
  gen_server:stop(?config(api_pid, Config)),
  Config.


%%%%%%%%%%%%%%%%%%
%%% TEST CASES %%%
%%%%%%%%%%%%%%%%%%

get_flos_sync_test(Config) ->
  Pid = ?config(api_pid, Config),
  {ok, Flos} = azq_api:get_flos(Pid),
  true = erlang:is_list(Flos).

get_flos_async_test(Config) ->
  Pid = ?config(api_pid, Config),
  Ref = azq_api:async_get_flos(Pid),
  {ok, Flos} = azq_api:yield_get_flos(Pid, Ref),
  true = erlang:is_list(Flos).

promise_get_flos_test(Config) ->
  {Ref, NState} = azq_api:promise_get_flos(?config(state, Config)),
  P = maps:get(Ref, NState#api_state.promises),
  init = P#promise.state,
  Ref = P#promise.cref,
  Config.
