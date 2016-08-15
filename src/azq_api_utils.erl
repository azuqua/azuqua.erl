-module(azq_api_utils).

-export([sign_data/5,
         create_timestamp/0,
         construct_url/3,
         construct_headers/2]).

sign_data(Secret, Data, Verb, Path, Time) ->
  NVerb = azq_api_bin:to_lower_case(Verb),
  Prefix = azq_api_bin:join([NVerb, Path, Time], <<":">>),
  NData = <<Prefix/binary, Data/binary>>,
  azq_api_bin:to_hex(crypto:hmac(sha256, Secret, NData)).

create_timestamp() ->
  erlang:integer_to_binary(erlang:system_time(milli_seconds)).

construct_url(_Opts, _Path, _QS) ->
  ok.

construct_headers(_Opts, _Headers) ->
  ok.
