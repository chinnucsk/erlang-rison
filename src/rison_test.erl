-module(rison_test).

-export([all/0]).
-export([encode/0]).
-export([decode/0]).
-export([invalid/0]).


all() ->
  ok = encode(),
  ok = decode(),
  ok = invalid(),
  ok.

encode() ->
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
    , {"Iñtërnâtiônàlizætiøn", "'Iñtërnâtiônàlizætiøn'"}
    , {{object, []}, "()"}
    , {{object, [{'a',0}]}, "(a:0)"}
    , {{object, [{'id',undefined}, {'type','/common/document'}]}, "(id:!n,type:/common/document)"}
    , {{object, [{"id",undefined}, {"type","/common/document"}]}, "(id:!n,type:/common/document)"}
    , {{array, []}, "!()"}
    , {{array, [true,false,undefined,""]}, "!(!t,!f,!n,'')"}
  ]).

decode() ->
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
    , {"'Iñtërnâtiônàlizætiøn'", "Iñtërnâtiônàlizætiøn"}
    , {"()", {object, []}}
    , {"(a:0)", {object, [{a,0}]}}
    , {"(id:!n,type:/common/document)", {object, [{id,undefined}, {type,'/common/document'}]}}
    , {"!()", {array, []}}
    , {"!(!t,!f,!n,'')", {array, [true,false,undefined,""]}}
  ]).

invalid() ->
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
  case apply(rison, F, [Input]) of
    Expected ->
      true;
    _ ->
      false
  end;
test_case(F, raises_error, Input) ->
  try
    apply(rison, F, [Input]),
    false
  catch
    error:_ ->
      true
  end.
