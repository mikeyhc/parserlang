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

oneof_test_() -> [ ?_assertEqual({$x, <<"yz">>},
                                 parserlang:oneof(<<"abcxyz">>, <<"xyz">>)),
                   ?_assertThrow({parse_error, expected, "one of \"abc\""},
                                 parserlang:oneof(<<"abc">>, <<"xyz">>)),
                   ?_assertError({badarg, a}, parserlang:oneof(a, <<>>)),
                   ?_assertError({badarg, a}, parserlang:oneof(<<>>, a))
                 ].

%%%%%%%%%%%%%%%%%%%%%%%
%%% Generic Parsers %%%
%%%%%%%%%%%%%%%%%%%%%%%
% test case sensitive charcter matches
char_test_() ->
    [ lists:map(
        fun(X) -> ?_assertEqual({X, <<>>}, parserlang:char(X, <<X>>)) end,
        [ $A, $a, $Z, $z, $:, $@, $$ ]),
      ?_assertThrow({parse_error, expected, "a"},
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
            ?_assertThrow({parse_error, expected, [X]},
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

% test string matches
string_test_() ->
    [ ?_assertEqual({<<"abc">>, <<>>},
                    parserlang:string(<<"abc">>, <<"abc">>)),
      ?_assertEqual({<<"abc">>, <<"d">>},
                    parserlang:string(<<"abc">>, <<"abcd">>)),
      ?_assertThrow({parse_error, expected, "D"},
                    parserlang:string(<<"abcD">>, <<"abcd">>)),
      ?_assertThrow({parse_error, expected, <<"D">>},
                    parserlang:string(<<"abcD">>, <<"abc">>)),
      ?_assertError({badarg, a}, parserlang:string(a, <<>>)),
      ?_assertError({badarg, a}, parserlang:string(<<>>, a))
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser Combinators %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

many_test_() ->
    F = fun test_parser/1,
    [?_assertEqual({[], <<>>}, parserlang:many(F, <<>>)),
     ?_assertEqual({[], <<"a">>}, parserlang:many(F, <<"a">>)),
     ?_assertEqual({[0,1], <<>>}, parserlang:many(F, <<0, 1>>))
    ].

many1_test_() ->
    F = fun test_parser/1,
    [?_assertThrow({parse_error, expected, _}, parserlang:many1(F, <<"a">>)),
     ?_assertEqual({[0], <<$a>>}, parserlang:many1(F, <<0, $a>>)),
     ?_assertEqual({[0,1], <<>>}, parserlang:many1(F, <<0,1>>))
    ].

option_test_() ->
    F = fun(X) -> parserlang_tests:test_parser(X) end,
    [?_assertEqual({<<>>, <<"a">>}, parserlang:option(<<>>, F, <<"a">>)),
     ?_assertEqual({0, <<>>}, parserlang:option(<<>>, F, <<0>>)),
     ?_assertEqual({<<>>, <<>>}, parserlang:option(<<>>, F, <<>>))
    ].

either_test_() -> [ ?_assertEqual({0, <<>>},
                                  parserlang:either(fun test_parser/1,
                                                    fun test_parser2/1,
                                                    <<0>>, "msg")),
                    ?_assertEqual({11, <<>>},
                                  parserlang:either(fun test_parser/1,
                                                    fun test_parser2/1,
                                                    <<11>>, "msg")),
                    ?_assertThrow({parse_error, expected, "msg"},
                                  parserlang:either(fun test_parser/1,
                                                    fun test_parser2/1,
                                                    <<"a">>, "msg")),
                    ?_assertThrow({parse_error, expected, "msg"},
                                  parserlang:either(fun test_parser/1,
                                                    fun test_parser2/1,
                                                    <<>>, "msg"))
                  ].


both_test_() -> [ ?_assertThrow({parse_error, expected, "msg"},
                                parserlang:both(fun test_parser/1,
                                                fun test_parser/1,
                                                <<"a", 0>>, "msg")),
                  ?_assertThrow({parse_error, expected, "msg"},
                                parserlang:both(fun test_parser/1,
                                                fun test_parser/1,
                                                <<0, "a">>, "msg")),
                  ?_assertThrow({parse_error, expected, "msg"},
                                parserlang:both(fun test_parser/1,
                                                fun test_parser/1,
                                                <<>>, "msg")),
                  ?_assertEqual({{0,1}, <<>>},
                                parserlang:both(fun test_parser/1,
                                                fun test_parser/1,
                                                <<0, 1>>, "msg"))
                ].

optional_test_() -> [ ?_assertEqual({<<>>, <<"a">>},
                                    parserlang:optional(fun test_parser/1,
                                                        <<"a">>)),
                      ?_assertEqual({0, <<>>},
                                    parserlang:optional(fun test_parser/1,
                                                        <<0>>))
                    ].

between_test_() ->
    HF = fun(X) -> parserlang:case_string(<<"a">>, X) end,
    TF = match_fun(<<"|">>),
    BC = fun(X) -> parserlang:case_string(<<"bc">>, X) end,
    [?_assertEqual({<<"bc">>, <<"xyz">>},
                   parserlang:between(HF, TF, BC, <<"abc|xyz">>)),
     ?_assertThrow({parse_error, expected, "a"},
                   parserlang:between(HF, TF, BC, <<"bc|xyz">>)),
     ?_assertThrow({parse_error, expected, <<"|">>},
                   parserlang:between(HF, TF, BC, <<"abcxyz">>)),
     ?_assertError({badarg, a}, parserlang:between(a, TF, BC, <<>>)),
     ?_assertError({badarg, a}, parserlang:between(HF, a, BC, <<>>)),
     ?_assertError({badarg, a}, parserlang:between(HF, TF, a, <<>>))
   ].

until_test_() -> [ ?_assertEqual({<<"abc">>, <<"xyz">>},
                                 parserlang:until(match_fun(<<"|">>),
                                                  <<"abc|xyz">>)),
                   ?_assertEqual({$a, <<"bc">>},
                                 parserlang:until(
                                   fun(Y) ->
                                           <<H,T/binary>> = Y,
                                           {H, T}
                                   end, <<"abc">>)),
                   ?_assertThrow({parse_error, expected, <<"|">>},
                                 parserlang:until(match_fun(<<"|">>),
                                                  <<"abcxyz">>)),
                   ?_assertError({badarg, a}, parserlang:until(a, <<>>)),
                   ?_assertError({badarg, a}, parserlang:until(match_fun(<<>>),
                                                               a))
                 ].

tryparse_test_() -> [ ?_assertEqual({0, <<>>},
                                    parserlang:tryparse(fun test_parser/1,
                                                        <<0>>)),
                      ?_assertEqual({<<>>, <<11>>},
                                    parserlang:tryparse(fun test_parser/1,
                                                        <<11>>)),
                      ?_assertEqual({<<>>, <<>>},
                                    parserlang:tryparse(fun test_parser/1,
                                                        <<>>))
                    ].

orparse_test_() ->
    F1 = fun test_parser/1,
    F2 = fun test_parser2/1,
    [ ?_assertEqual({11, <<>>},
                    parserlang:orparse([{parserlang_tests, test_parser},
                                        {parserlang_tests, test_parser2}],
                                       <<11>>, "char between 0 and 20")),
      ?_assertThrow({parse_error, expected, "char between 0 and 20"},
                    parserlang:orparse([{parserlang_tests, test_parser},
                                        {parserlang_tests, test_parser2}],
                                       <<21>>, "char between 0 and 20")),
      ?_assertThrow({parse_error, expected, "char between 0 and 20"},
                    parserlang:orparse([{parserlang_tests, test_parser},
                                        {parserlang_tests, test_parser2}],
                                       <<>>, "char between 0 and 20")),
      ?_assertEqual({11, <<>>},
                    parserlang:orparse([F1, F2], <<11>>,
                                       "char between 0 and 20")),
      ?_assertThrow({parse_error, expected, "char between 0 and 20"},
                    parserlang:orparse([F1, F2], <<21>>,
                                       "char between 0 and 20")),
      ?_assertThrow({parse_error, expected, "char between 0 and 20"},
                    parserlang:orparse([F1, F2], <<>>,
                                       "char between 0 and 20")),
      ?_assertError({badarg, a}, parserlang:orparse(a, <<>>, ""))
    ].

choice_test_() ->
    C = parserlang:choice([ fun test_parser/1, fun test_parser2/1 ],
                          "char between 0 and 20"),
    [ ?_assertEqual({11, <<>>}, C(<<11>>)),
      ?_assertThrow({parse_error, expected, "char between 0 and 20"},
                    C(<<21>>)),
      ?_assertThrow({parse_error, expected, "char between 0 and 20"}, C(<<>>)),
      ?_assertError({badarg, a}, C(a))
    ].

sepby_test_() ->
    F1 = fun test_parser/1,
    F2 = fun test_parser2/1,
    [ ?_assertEqual({[1,1], <<>>},
                    parserlang:sepby(F1, F2, <<1, 11, 1>>)),
      ?_assertEqual({[], <<21>>},
                    parserlang:sepby(F1, F2, <<21>>)),
      ?_assertEqual({[], <<>>},
                    parserlang:sepby(F1, F2, <<>>)),
      ?_assertEqual({[10], <<21>>},
                    parserlang:sepby(F1, F2, <<10, 21>>))
    ].

sepby1_test_() ->
    F1 = fun test_parser/1,
    F2 = fun test_parser2/1,
    [ ?_assertEqual({[1,1], <<>>}, parserlang:sepby1(F1, F2, <<1,11,1>>))
    ].

count_test_() ->
    F = fun test_parser/1,
    [ ?_assertEqual({[1,2,3], <<>>},
                    parserlang:count(3, F, <<1,2,3>>)),
      ?_assertEqual({[1,2], <<3>>},
                    parserlang:count(2, F, <<1,2,3>>)),
      ?_assertError({badarg, a}, parserlang:count(a, a, a)),
      ?_assertError({badarg, a}, parserlang:count(2, a, a)),
      ?_assertError({badarg, a}, parserlang:count(2, F, a))
    ].

manyN_test_() ->
    F = fun test_parser/1,
    [ ?_assertEqual({[1,2,3], <<>>},
                    parserlang:manyN(3, F, <<1,2,3>>)),
      ?_assertEqual({[1,2,3], <<>>},
                    parserlang:manyN(2, F, <<1,2,3>>)),
      ?_assertError({badarg, a}, parserlang:manyN(a, a, a)),
      ?_assertError({badarg, a}, parserlang:manyN(2, a, a)),
      ?_assertError({badarg, a}, parserlang:manyN(2, F, a))
    ].

manyNtoM_test_() ->
    F = fun test_parser/1,
    [ ?_assertEqual({[1,2,3], <<>>},
                    parserlang:manyNtoM(2, 4, F, <<1,2,3>>)),
      ?_assertEqual({[1,2,3,4], <<5>>},
                    parserlang:manyNtoM(2, 4, F, <<1,2,3,4,5>>)),
      ?_assertEqual({[], <<>>}, parserlang:manyNtoM(-1, 4, F, <<>>)),
      ?_assertEqual({[], <<>>}, parserlang:manyNtoM(4, 0, F, <<>>)),
      ?_assertEqual({[0,1,2], <<>>},
                    parserlang:manyNtoM(3, 3, F, <<0,1,2>>)),
      ?_assertEqual({[1], <<>>},
                    parserlang:manyNtoM(1, 2, F, <<1>>)),
      ?_assertEqual({[1], <<12>>},
                    parserlang:manyNtoM(1, 2, F, <<1,12>>))
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
