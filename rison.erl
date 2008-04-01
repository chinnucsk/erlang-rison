% Copyright (c) 2008 Tim Fletcher <tfletcher.com>
% 
% Permission is hereby granted, free of charge, to any person
% obtaining a copy of this software and associated documentation
% files (the "Software"), to deal in the Software without
% restriction, including without limitation the rights to use,
% copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following
% conditions:
% 
% The above copyright notice and this permission notice shall be
% included in all copies or substantial portions of the Software.
% 
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
% OTHER DEALINGS IN THE SOFTWARE.

-module(rison).
-vsn(20080401).

-export([load/1]).
-export([dump/1]).
-export([encode/1]).
-export([decode/1]).
-export([test/1]).

-define(is_digit(C), C >= $0, C =< $9).
-define(is_alpha(C), C >= $A, C =< $Z; C >= $a, C =< $z).
-define(is_1to9(C), C >= $1, C =< $9).
-define(is_idstart(C), ?is_alpha(C); C =:= $_; C =:= $.; C =:= $/; C =:= $~).
-define(is_idchar(C), ?is_idstart(C); ?is_digit(C); C =:= $-).


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
  "!(" ++ implode($,, lists:map(fun encode/1, Values)) ++ ")".

encode_object(Attrs) ->
  "(" ++ implode($,, lists:map(fun encode_object_pair/1, Attrs)) ++ ")".

encode_object_pair({Key, Value}) ->
  implode($:, [encode(Key), encode(Value)]).

implode(Sep, Strings) when is_integer(Sep) ->
  lists:foldr(fun(Str, []) -> Str; (Str, Acc) -> Str ++ [Sep|Acc] end, [], Strings).

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


test(F, Assertion, TestCases) ->
  test(F, Assertion, TestCases, 0).

test(F, Assertion, [], N) ->
  io:format("Tested ~p ~p (~p test cases)~n", [F, Assertion, N]), ok;
test(F, Assertion, [TestCase|Etc], N) ->
  case test_case(F, Assertion, TestCase) of
    true ->
      test(F, Assertion, Etc, N + 1);
    false ->
      io:format("Test failure: ~p ~p: test case #~p~n", [F, Assertion, N + 1])
  end.

test_case(F, equals_expected, {Input, Expected}) ->
  case apply(?MODULE, F, [Input]) of
    Expected ->
      true;
    _ ->
      false
  end;
test_case(F, raises_error, Input) ->
  try
    apply(?MODULE, F, [Input]),
    false
  catch
    error:_ ->
      true
  end.


test(all) ->
  ok = test(encode),
  ok = test(decode),
  ok = test(invalid),
  ok;
test(encode) ->
  test(encode, equals_expected, [
      {true, "!t"}
    , {false, "!f"}
    , {undefined, "!n"}
    , {0, "0"}
    , {42, "42"}
    , {-42, "-42"}
    , {{number, 1, 5, undefined}, "1.5"}
    , {{number, 99, 99, undefined}, "99.99"}
    , {{number, 99, '09', undefined}, "99.09"}
    , {{number, 1, undefined, 30}, "1e30"}
    , {{number, 1, undefined, -30}, "1e-30"}
    , {{number, 1, '5', 2}, "1.5e2"}
    , {{number, 1, '5', -2}, "1.5e-2"}
    , {'a', "a"}
    , {'a-z', "a-z"}
    , {'domain.com', "domain.com"}
    , {"", "''"}
    , {"0a", "'0a'"}
    , {"-h", "'-h'"}
    , {"can't", "'can!'t'"}
    , {"wow!", "'wow!!'"}
    , {"abc def", "'abc def'"}
    , {"user@domain.com", "'user@domain.com'"}
    , {"US $10", "'US $10'"}
    , {{object, []}, "()"}
    , {{object, [{'a',0}]}, "(a:0)"}
    , {{object, [{'id',undefined}, {'type','/common/document'}]}, "(id:!n,type:/common/document)"}
    , {{object, [{"id",undefined}, {"type","/common/document"}]}, "(id:!n,type:/common/document)"}
    , {{array, []}, "!()"}
    , {{array, [true,false,undefined,""]}, "!(!t,!f,!n,'')"}
  ]);
test(decode) ->
  test(decode, equals_expected, [
      {"!t", true}
    , {"!f", false}
    , {"!n", undefined}
    , {"0", 0}
    , {"1", 1}
    , {"12", 12}
    , {"-3", -3}
    , {"-33", -33}
    , {"1.5", {number, 1, '5', undefined}}
    , {"99.99", {number, 99, '99', undefined}}
    , {"99.09", {number, 99, '09', undefined}}
    , {"1e30", {number, 1, undefined, 30}}
    , {"1e-30", {number, 1, undefined, -30}}
    , {"1.5e2", {number, 1, '5', 2}}
    , {"1.5e-2", {number, 1, '5', -2}}
    , {"a", a}
    , {"a-z", 'a-z'}
    , {"domain.com", 'domain.com'}
    , {"''", ""}
    , {"'0a'", "0a"}
    , {"'-h'", "-h"}
    , {"'can!'t'", "can't"}
    , {"'wow!!'", "wow!"}
    , {"'abc def'", "abc def"}
    , {"'user@domain.com'", "user@domain.com"}
    , {"'US $10'", "US $10"}
    , {"()", {object, []}}
    , {"(a:0)", {object, [{a,0}]}}
    , {"(id:!n,type:/common/document)", {object, [{id,undefined}, {type,'/common/document'}]}}
    , {"!()", {array, []}}
    , {"!(!t,!f,!n,'')", {array, [true,false,undefined,""]}}
  ]);
test(invalid) ->
  test(decode, raises_error, [
      "-h"
    , "1.5e+2"
    , "1.5E2"
    , "1.5E+2"
    , "1.5E-2"
    , "abc def"
    , "US $10"
    , "user@domain.com"
  ]).