-module(azq_api_bin).

-export([join/2,
         to_lower_case/1,
         to_hex/1]).

join(List, Del) when is_binary(Del) ->
  join(List, Del, <<>>).

join([], _Del, Acc) ->
  Acc;
join([H | T], Del, Acc) ->
  join(T, Del, <<Acc/binary, Del/binary, H/binary>>).

to_lower_case(Bin) when is_binary(Bin) ->
  << <<(char_to_lower(A))>> || <<A:8>> <= Bin>>.

char_to_lower(C) when C >= $a andalso C =< $z ->
  C;
char_to_lower(C) when C >= $A andalso C =< $Z ->
  C - 32;
char_to_lower(C) ->
  C.

to_hex(Bin) ->
  << <<(hex(A)), (hex(B))>> || <<A:4, B:4>> <= Bin>>.

hex(A) when A < 10 ->
  $0 + A;
hex(A) ->
  A + $a -10.
