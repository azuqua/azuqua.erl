-module(azq_api_utils).

-export([sign_data/5,
         create_timestamp/0,
         construct_url/2,
         construct_headers/2,
         add_base/2,
         add_content_length/3,
         add_timestamp/2,
         add_key/2,
         add_hash/2]).

-include("azq_api.hrl").

sign_data(Secret, Data, Verb, Path, Time) ->
  NVerb = verb_to_bin(Verb),
  Prefix = azq_api_bin:join([NVerb, Path, Time], <<":">>),
  NData = <<Prefix/binary, Data/binary>>,
  azq_api_bin:to_hex(crypto:hmac(sha256, Secret, NData)).

verb_to_bin(get) ->
  <<"get">>;
verb_to_bin(post) ->
  <<"post">>;
verb_to_bin(put) ->
  <<"put">>;
verb_to_bin(delete) ->
  <<"delete">>.

create_timestamp() ->
  {{Y, M, D}, {H, MM, S}} = calendar:universal_time(),
  Format = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
  F = io_lib:format(Format, [Y, M, D, H, MM, S]),
  erlang:list_to_binary(lists:flatten(F)).

construct_url(#request{path=P, qs=QS}, #api_state{opts=O}) ->
  Base = maps:get(base, O, <<>>),
  <<Base/binary, P/binary, QS/binary>>.

construct_headers(#request{method=M, body=B, headers=H0, path=P}, State) ->
  T = create_timestamp(),
  Hash = sign_data(State#api_state.secret, B, M, P, T),
  H = add_base(State#api_state.opts, H0),
  H1 = add_content_length(M, B, H),
  H2 = add_timestamp(T, H1),
  H3 = add_key(State#api_state.key, H2),
  add_hash(Hash, H3).

add_base(Opts, Headers) ->
  lists:append([maps:get(headers, Opts, []), Headers]).

add_content_length(get, _Body, Headers) ->
  Headers;
add_content_length(post, Body, Headers) ->
  [{<<"Content-Length">>, erlang:size(Body)} | Headers].

add_timestamp(T, Headers) ->
  [{<<"x-api-timestamp">>, T} | Headers].

add_key(Key, Headers) ->
  [{<<"x-api-accessKey">>, Key} | Headers].

add_hash(Hash, Headers) ->
  [{<<"x-api-hash">>, Hash} | Headers].
