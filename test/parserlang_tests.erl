%% parserlang_tests.erl
%% Unit tests for the parser combinators provided by parserlang.erl
%%
%% author: mikeyhc <mikeyhc@atmosia.net>

-module(parserlang_tests).
-include_lib("eunit/include/eunit.hrl").
-export([test_parser/1, test_parser2/1, match_fun/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper Functions %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%

test_parser(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H >= 0 andalso H =< 10 -> {H, T};
       true -> throw({parse_error, expected, "between 0 and 10"})
    end;
test_parser(X) -> error({badarg, X}).

test_parser2(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H >= 11 andalso H =< 20 -> {H, T};
       true -> throw({parse_error, expected, "between 11 and 20"})
    end;
test_parser2(X) -> error({badarg, X}).

match_fun(X) ->
    fun(Y) ->
            case Y of
                <<>> -> throw({parse_error, expected, X});
                _    ->
                    <<H, T/binary>> = Y,
                    if <<H>> == X -> {match, T};
                       true -> nomatch
                    end
            end
    end.

% a list of the alphabet codes
alphaset() -> lists:concat([ lists:seq(65, 90), lists:seq(97, 122) ]).

%% the list of all common types which are not integers
noninteger_typeset() -> [ [], <<>>,  a, "", 0.0, fun() -> {} end, {a, typle} ].

%% the list of all common types which are not binary
nonbinary_typeset() -> [ [], 0, a, "", 0.0, fun() -> {} end, {a, tuple}].

%%%%%%%%%%%%%%%%%%%%%%%
%%% Generic Parsers %%%
%%%%%%%%%%%%%%%%%%%%%%%
% test case sensitive charcter matches
char_test_() ->
    [ lists:map(
        fun(X) -> ?_assertEqual({X, <<>>}, parserlang:char(X, <<X>>)) end,
        [ $A, $a, $Z, $z, $:, $@, $$ ]),
      ?_assertThrow({parse_error, expected, $a},
                    parserlang:char($a, <<"b">>)),
      ?_assertError({badarg, a}, parserlang:char($a, a)),
      ?_assertError({badarg, a}, parserlang:char(a, <<>>))
    ].

% test case insensitive charcter matches
case_char_test_() ->
    [ lists:map(
        fun(X) ->
            [ ?_assertEqual({X, <<>>}, parserlang:case_char(X, <<X>>)),
              ?_assertEqual({X + 32, <<>>},
                            parserlang:case_char(X, <<(X + 32)>>)) ]
        end,
        lists:seq(65, 90)),
      lists:map(fun(X) ->
        lists:map(fun(Y) ->
            ?_assertThrow({parse_error, expected, X},
                          parserlang:case_char(X, <<Y>>))
                  end,
                  lists:subtract(alphaset(), [X, X+32]))
                end,
                lists:seq(65, 90)),
      lists:map(
        fun(X) -> ?_assertError({badarg, _},
                                parserlang:case_char(X, <<>>)) end,
        noninteger_typeset()),
      lists:map(
        fun(X) -> ?_assertError({badarg, _}, parserlang:case_char(97, X)) end,
        nonbinary_typeset())
    ].

% test case insensitive string matches
case_string_test_() ->
    [ lists:map(fun({Match, In}) ->
                       ?_assertEqual({In, <<>>},
                       parserlang:case_string(Match, In))
               end,
               [ { <<"abc">>, <<"abc">> },
                 { <<"XyZ">>, <<"xyz">> },
                 { <<"ijk">>, <<"Ijk">> } ]),
      lists:map(fun({Match, In}) ->
                        ?_assertThrow({parse_error, expected, _},
                                      parserlang:case_string(Match, In))
                end,
                [ { <<"abc">>, <<"ab">> },
                  { <<"ihj">>, <<"abc">> },
                  { <<"abd">>, <<"abc">> } ]),
      lists:map(
        fun(X) -> ?_assertError({badarg, _},
                                parserlang:case_string(X, <<>>)) end,
        nonbinary_typeset()),
      lists:map(
        fun(X) -> ?_assertError({badarg, _},
                                parserlang:case_string(<<>>, X)) end,
        nonbinary_typeset())
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser Combinators %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

many_test_() -> [ ?_assertEqual({<<>>, <<>>},
                                parserlang:many(parserlang_tests, test_parser,
                                                <<>>)),
                  ?_assertEqual({<<>>, <<"a">>},
                                parserlang:many(parserlang_tests, test_parser,
                                                <<"a">>)),
                  ?_assertEqual({<<0,1>>, <<>>},
                                parserlang:many(parserlang_tests, test_parser,
                                                <<0,1>>)),
                  %% to test apply_many w/list
                  ?_assertEqual({<<0,1>>, <<>>},
                                parserlang:many(parserlang_tests, test_parser,
                                                [<<0,1>>]))
                ].

many1_test_() -> [ ?_assertThrow({parse_error, expected, _},
                                 parserlang:many1(parserlang_tests,
                                                  test_parser, <<"a">>)),
                   ?_assertEqual({<<0,1>>, <<>>},
                                 parserlang:many1(parserlang_tests,
                                                  test_parser, <<0,1>>))
                 ].

option_test_() -> [ ?_assertEqual({<<>>, <<"0">>},
                                  parserlang:option(<<>>, parserlang_tests,
                                                    test_parser, <<"0">>)),
                    ?_assertEqual({0, <<>>},
                                  parserlang :option(<<>>, parserlang_tests,
                                                     test_parser, <<0>>))
                  ].

either_test_() -> [ ?_assertEqual({0, <<>>},
                                  parserlang:either(parserlang_tests,
                                                    test_parser,
                                                    parserlang_tests,
                                                    test_parser2,
                                                    <<0>>, "msg")),
                    ?_assertEqual({11, <<>>},
                                  parserlang:either(parserlang_tests,
                                                    test_parser,
                                                    parserlang_tests,
                                                    test_parser2,
                                                    <<11>>, "msg")),
                    ?_assertThrow({parse_error, expected, "msg"},
                                  parserlang:either(parserlang_tests,
                                                    test_parser,
                                                    parserlang_tests,
                                                    test_parser2,
                                                    <<"a">>, "msg")),
                    ?_assertThrow({parse_error, expected, "msg"},
                                  parserlang:either(parserlang_tests,
                                                    test_parser,
                                                    parserlang_tests,
                                                    test_parser2,
                                                    <<>>, "msg"))
                  ].


both_test_() -> [ ?_assertThrow({parse_error, expected, "msg"},
                                parserlang:both(parserlang_tests, test_parser,
                                                parserlang_tests, test_parser,
                                                <<"a", 0>>, "msg")),
                  ?_assertThrow({parse_error, expected, "msg"},
                                parserlang:both(parserlang_tests, test_parser,
                                                parserlang_tests, test_parser,
                                                <<0, "a">>, "msg")),
                  ?_assertThrow({parse_error, expected, "msg"},
                                parserlang:both(parserlang_tests, test_parser,
                                                parserlang_tests, test_parser,
                                                <<>>, "msg")),
                  ?_assertEqual({<<0,1>>, <<>>},
                                parserlang:both(parserlang_tests, test_parser,
                                                parserlang_tests, test_parser,
                                                <<0, 1>>, "msg"))
                ].

optional_test_() -> [ ?_assertEqual({<<>>, <<"a">>},
                                    parserlang:optional(parserlang_tests,
                                                        test_parser,
                                                        <<"a">>)),
                      ?_assertEqual({0, <<>>},
                                    parserlang:optional(parserlang_tests,
                                                        test_parser,
                                                        <<0>>))
                    ].

between_test_() ->
    HF = fun(X) -> parserlang:case_string(<<"a">>, X) end,
    TF = match_fun(<<"|">>),
    [ ?_assertEqual({<<"bc">>, <<"xyz">>},
                                   parserlang:between(HF, TF,
                                                      binary, copy,
                                                      <<"abc|xyz">>)),
                     ?_assertThrow({parse_error, expected, <<"a">>},
                                   parserlang:between(HF, TF,
                                                      binary, copy,
                                                      <<"bc|xyz">>)),
                     ?_assertThrow({parse_error, expected, <<"|">>},
                                   parserlang:between(HF, TF,
                                                      binary, copy,
                                                      <<"abcxyz">>)),
                     ?_assertError({badarg, a},
                                   parserlang:between(a, TF, binary, copy,
                                                      <<>>)),
                     ?_assertError({badarg, a},
                                   parserlang:between(HF, a, binary, copy,
                                                      <<>>)),
                     ?_assertError({badarg, a},
                                   parserlang:between(HF, TF, binary, copy, a))
                   ].

until_test_() -> [ ?_assertEqual({<<"abc">>, <<"xyz">>},
                                 parserlang:until(match_fun(<<"|">>),
                                                  <<"abc|xyz">>)),
                   ?_assertThrow({parse_error, expected, <<"|">>},
                                 parserlang:until(match_fun(<<"|">>),
                                                  <<"abcxyz">>)),
                   ?_assertError({badarg, a}, parserlang:until(a, <<>>)),
                   ?_assertError({badarg, a}, parserlang:until(match_fun(<<>>),
                                                               a))
                 ].

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type Construction %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

bin_join_test_() ->
    [ ?_assertEqual(<<"abc">>, parserlang:bin_join(<<"a">>, <<"bc">>)),
      ?_assertEqual(<<"abc">>, parserlang:bin_join(97, <<"bc">>)),
      ?_assertEqual(<<"abc">>, parserlang:bin_join(<<"ab">>, 99)),
      ?_assertEqual(<<"aa">>, parserlang:bin_join(97, 97))
    ].

bin_concat_test_() ->
    ?_assertEqual(<<"abcd">>, parserlang:bin_concat([ 97, <<"bc">>, 100])).
