-module(azq_api_bin_tests).

-include_lib("eunit/include/eunit.hrl").

join_test_() ->
  join().

to_lower_case_test_() ->
  to_lower_case().

to_hex_test_() ->
  to_hex().


join() ->
  F1 = azq_api_bin:join([<<"a">>, <<"b">>, <<"c">>], <<":">>),
  ?assertEqual(F1, <<"a:b:c">>),
  F2 = azq_api_bin:join([], <<":">>),
  ?_assertEqual(F2, <<>>).

to_lower_case() ->
  F1 = azq_api_bin:to_lower_case(<<"ABC">>),
  ?_assertEqual(F1, <<"abc">>).

to_hex() ->
  F1 = azq_api_bin:to_hex(<<"abc">>),
  ?_assertEqual(F1, <<"616263">>).
