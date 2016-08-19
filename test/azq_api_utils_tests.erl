-module(azq_api_utils_tests).

-include("../src/azq_api.hrl").
-include_lib("eunit/include/eunit.hrl").

sign_data_test_() ->
  sign_data().

construct_url_test_() ->
  construct_url().

construct_headers_test_() ->
  construct_headers().

sign_data() ->
  Secret = <<"foo">>,
  Data = <<"bar">>,
  Verb = get,
  Path = <<"path">>,
  Time = <<"t">>,
  Exp = azq_api_utils:sign_data(Secret, Data, Verb, Path, Time),
  Act = azq_api_bin:to_hex(crypto:hmac(sha256, Secret, <<"get:path:tbar">>)),
  ?_assertEqual(Exp, Act).

construct_url() ->
  Req = #request{path = <<"path">>, qs = <<"qs">>},
  State = #api_state{opts = #{base => <<"base">>}},
  F1 = azq_api_utils:construct_url(Req, State),
  ?_assertEqual(F1, <<"basepathqs">>).

construct_headers() ->
  Opts = #{base => <<"base">>},
  Req = #request{method = post, body = <<"b">>, headers = [], path = <<"p">>},
  State = #api_state{secret = <<"secret">>, key = <<"key">>, opts = Opts},
  F1 = azq_api_utils:construct_headers(Req, State),
  ?assertEqual(proplists:get_value(<<"x-api-accessKey">>, F1), <<"key">>),
  ?_assertEqual(proplists:get_value(<<"Content-Length">>, F1), 1).
