-module(rison).

-export([load/1, dump/1, encode/1, decode/1]).

-define(is_digit(C), C >= $0, C =< $9).
-define(is_alpha(C), C >= $A, C =< $Z; C >= $a, C =< $z).
-define(is_1to9(C), C >= $1, C =< $9).
-define(is_idstart(C), ?is_alpha(C); C =:= $_; C =:= $.; C =:= $/; C =:= $~).
-define(is_idchar(C), ?is_idstart(C); ?is_digit(C); C =:= $-).

-include_lib("eunit/include/eunit.hrl").

load(Input) ->
  catch_invalid_input(fun() -> decode(Input) end).

dump(Input) ->
  catch_invalid_input(fun() -> encode(Input) end).

catch_invalid_input(F) ->
  try {ok, F()} catch error:_ -> {error, invalid_input} end.

encode(true) ->
  "!t";
encode(false) ->
  "!f";
encode(undefined) ->
  "!n";
encode(N) when is_integer(N) ->
  integer_to_list(N);
encode({number, Int, Frac, Exp}) ->
  encode_number(Int, Frac, Exp);
encode({array, []}) ->
  "!()";
encode({array, Values}) ->
  encode_array(Values);
encode({object, []}) ->
  "()";
encode({object, Attrs}) ->
  encode_object(Attrs);
encode(Atom) when is_atom(Atom) ->
  encode_string_or_id(atom_to_list(Atom));
encode(Input) when is_list(Input) ->
  encode_string_or_id(Input).

encode_number(Int, Frac, Exp) ->
  integer_to_list(Int) ++ encode_frac(Frac) ++ encode_exp(Exp).

encode_frac(undefined) ->
  [];
encode_frac(N) when is_atom(N) ->
  [$.|atom_to_list(N)];
encode_frac(N) when is_integer(N) ->
  [$.|integer_to_list(N)].

encode_exp(undefined) ->
  [];
encode_exp(N) when is_integer(N) ->
  [$e|integer_to_list(N)].

encode_array(Values) ->
  "!(" ++ string:join(lists:map(fun encode/1, Values), ",") ++ ")".

encode_object(Attrs) ->
  "(" ++ string:join(lists:map(fun encode_object_pair/1, Attrs), ",") ++ ")".

encode_object_pair({Key, Value}) ->
  string:join([encode(Key), encode(Value)], ":").

encode_string_or_id([]) -> "''";
encode_string_or_id(Input) ->
  case catch encode_id(Input) of
    Chars when is_list(Chars) ->
      Chars;
    _ ->
      encode_string(Input)
  end.

encode_id(Input=[C|_]) when ?is_idstart(C) ->
  encode_id(Input, []).

encode_id([], Chars) ->
  lists:reverse(Chars);
encode_id([C|Etc], Chars) when ?is_idchar(C) ->
  encode_id(Etc, [C|Chars]).

encode_string(Input) ->
  encode_string(Input, [$']).

encode_string([], Chars) ->
  lists:reverse([$'|Chars]);
encode_string([$'|Etc], Chars) ->
  encode_string(Etc, [$',$!|Chars]);
encode_string([$!|Etc], Chars) ->
  encode_string(Etc, [$!,$!|Chars]);
encode_string([C|Etc], Chars) ->
  encode_string(Etc, [C|Chars]).


decode("!t") ->
  true;
decode("!f") ->
  false;
decode("!n") ->
  undefined;
decode("0") ->
  0;
decode(Input=[C|_]) when ?is_1to9(C) ->
  decode_number(Input);
decode(Input=[$-,C|_]) when ?is_1to9(C) ->
  decode_number(Input);
decode(Input=[C|_]) when ?is_idstart(C) ->
  decode_id(Input);
decode([$'|Etc]) ->
  decode_string(Etc);
decode(Input=[$(|_]) ->
  decode_object(Input);
decode(Input=[$!,$(|_]) ->
  decode_array(Input).

decode_number(Input) ->
  decode_int(Input).

decode_int(Input) when is_list(Input) ->
  decode_int(take_int(Input));
decode_int({Int, []}) ->
  Int;
decode_int({Int, [$e|Etc]}) ->
  decode_exp(Etc, Int, undefined);
decode_int({Int, [$.|Etc]}) ->
  decode_frac(Etc, Int).

decode_frac(Input, Int) when is_list(Input) ->
  decode_frac(take_digits(Input), Int);
decode_frac({Frac, []}, Int) ->
  {number, Int, list_to_atom(Frac), undefined};
decode_frac({Frac, [$e|Etc]}, Int) ->
  decode_exp(Etc, Int, list_to_atom(Frac)).

decode_exp(Input, Int, Frac) ->
  {Exp, []} = take_int(Input),
  {number, Int, Frac, Exp}.

take_int([$-|Etc]) ->
  {Int, Rem} = take_int(Etc), {-Int, Rem};
take_int([C|Etc]) when ?is_1to9(C) ->
  {Digits, Rem} = take_digits(Etc), {list_to_integer([C|Digits]), Rem}.

take_digits(Input) ->
  lists:splitwith(fun is_digit/1, Input).

is_digit(C) when ?is_digit(C) ->
  true;
is_digit(_) ->
  false.

decode_id(Input) ->
  decode_id(Input, []).

decode_id([], Acc) ->
  list_to_atom(lists:reverse(Acc));
decode_id([C|Etc], Acc) when ?is_idchar(C) ->
  decode_id(Etc, [C|Acc]).

decode_string(Input) ->
  decode_string(Input, []).

decode_string([$'], Acc) ->
  lists:reverse(Acc);
decode_string([$!, $!|Etc], Acc) ->
  decode_string(Etc, [$!|Acc]);
decode_string([$!, $'|Etc], Acc) ->
  decode_string(Etc, [$'|Acc]);
decode_string([C|Etc], Acc) ->
  decode_string(Etc, [C|Acc]).

decode_object("()") ->
  {object, []};
decode_object([$(|Etc]) ->
  decode_object(Etc, []).

decode_object([$)], Attrs) ->
  {object, lists:reverse(Attrs)};
decode_object([$,|Etc], Attrs) ->
  decode_object(Etc, Attrs);
decode_object(Input, Attrs) when is_list(Input) ->
  {Attr, Etc} = decode_object_pair(Input),
  decode_object(Etc, [Attr|Attrs]).

decode_object_pair(Input) ->
  {Key, [$:|Etc]} = lists:splitwith(fun(C) -> C =/= $: end, Input),
  decode_object_pair(Etc, decode_id(Key)).

decode_object_pair(Input, Key) ->
  {Value, Etc} = take_value(Input),
  {{Key, Value}, Etc}.

decode_array("!()") ->
  {array, []};
decode_array([$!,$(|Etc]) ->
  decode_array(Etc, []).

decode_array([$)], Values) ->
  {array, lists:reverse(Values)};
decode_array([$,|Etc], Values) ->
  decode_array(Etc, Values);
decode_array(Input, Values) ->
  {Value, Etc} = take_value(Input),
  decode_array(Etc, [Value|Values]).

take_value(Input) ->
  {Value, Etc} = lists:splitwith(fun(C) -> C =/= $, andalso C =/= $) end, Input),
  {decode(Value), Etc}.

encode_test_() -> [
    ?_assertEqual("!t", encode(true))
  , ?_assertEqual("!f", encode(false))
  , ?_assertEqual("!n", encode(undefined))
  , ?_assertEqual("0", encode(0))
  , ?_assertEqual("42", encode(42))
  , ?_assertEqual("-42", encode(-42))
  , ?_assertEqual("1.5", encode({number, 1, 5, undefined}))
  , ?_assertEqual("99.99", encode({number, 99, 99, undefined}))
  , ?_assertEqual("99.09", encode({number, 99, '09', undefined}))
  , ?_assertEqual("1e30", encode({number, 1, undefined, 30}))
  , ?_assertEqual("1e-30", encode({number, 1, undefined, -30}))
  , ?_assertEqual("1.5e2", encode({number, 1, '5', 2}))
  , ?_assertEqual("1.5e-2", encode({number, 1, '5', -2}))
  , ?_assertEqual("a", encode('a'))
  , ?_assertEqual("a-z", encode('a-z'))
  , ?_assertEqual("domain.com", encode('domain.com'))
  , ?_assertEqual("''", encode(""))
  , ?_assertEqual("'0a'", encode("0a"))
  , ?_assertEqual("'-h'", encode("-h"))
  , ?_assertEqual("'can!'t'", encode("can't"))
  , ?_assertEqual("'wow!!'", encode("wow!"))
  , ?_assertEqual("'abc def'", encode("abc def"))
  , ?_assertEqual("'user@domain.com'", encode("user@domain.com"))
  , ?_assertEqual("'US $10'", encode("US $10"))
  , ?_assertEqual("'Iñtërnâtiônàlizætiøn'", encode("Iñtërnâtiônàlizætiøn"))
  , ?_assertEqual("()", encode({object, []}))
  , ?_assertEqual("(a:0)", encode({object, [{'a',0}]}))
  , ?_assertEqual("(id:!n,type:/common/document)", encode({object, [{'id',undefined}, {'type','/common/document'}]}))
  , ?_assertEqual("(id:!n,type:/common/document)", encode({object, [{"id",undefined}, {"type","/common/document"}]}))
  , ?_assertEqual("!()", encode({array, []}))
  , ?_assertEqual("!(!t,!f,!n,'')", encode({array, [true,false,undefined,""]}))
  ].

decode_test_() -> [
    ?_assertEqual(true, decode("!t"))
  , ?_assertEqual(false, decode("!f"))
  , ?_assertEqual(undefined, decode("!n"))
  , ?_assertEqual(0, decode("0"))
  , ?_assertEqual(1, decode("1"))
  , ?_assertEqual(12, decode("12"))
  , ?_assertEqual(-3, decode("-3"))
  , ?_assertEqual(-33, decode("-33"))
  , ?_assertEqual({number, 1, '5', undefined}, decode("1.5"))
  , ?_assertEqual({number, 99, '99', undefined}, decode("99.99"))
  , ?_assertEqual({number, 99, '09', undefined}, decode("99.09"))
  , ?_assertEqual({number, 1, undefined, 30}, decode("1e30"))
  , ?_assertEqual({number, 1, undefined, -30}, decode("1e-30"))
  , ?_assertEqual({number, 1, '5', 2}, decode("1.5e2"))
  , ?_assertEqual({number, 1, '5', -2}, decode("1.5e-2"))
  , ?_assertEqual(a, decode("a"))
  , ?_assertEqual('a-z', decode("a-z"))
  , ?_assertEqual('domain.com', decode("domain.com"))
  , ?_assertEqual("", decode("''"))
  , ?_assertEqual("0a", decode("'0a'"))
  , ?_assertEqual("-h", decode("'-h'"))
  , ?_assertEqual("can't", decode("'can!'t'"))
  , ?_assertEqual("wow!", decode("'wow!!'"))
  , ?_assertEqual("abc def", decode("'abc def'"))
  , ?_assertEqual("user@domain.com", decode("'user@domain.com'"))
  , ?_assertEqual("US $10", decode("'US $10'"))
  , ?_assertEqual("Iñtërnâtiônàlizætiøn", decode("'Iñtërnâtiônàlizætiøn'"))
  , ?_assertEqual({object, []}, decode("()"))
  , ?_assertEqual({object, [{a,0}]}, decode("(a:0)"))
  , ?_assertEqual({object, [{id,undefined}, {type,'/common/document'}]}, decode("(id:!n,type:/common/document)"))
  , ?_assertEqual({array, []}, decode("!()"))
  , ?_assertEqual({array, [true,false,undefined,""]}, decode("!(!t,!f,!n,'')"))
  ].

decode_invalid_input_test_() -> [
    ?_assertError(_, decode("-h"))
  , ?_assertError(_, decode("1.5e+2"))
  , ?_assertError(_, decode("1.5E2"))
  , ?_assertError(_, decode("1.5E+2"))
  , ?_assertError(_, decode("1.5E-2"))
  , ?_assertError(_, decode("abc def"))
  , ?_assertError(_, decode("US $10"))
  , ?_assertError(_, decode("user@domain.com"))
  ].
