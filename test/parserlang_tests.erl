%% parserlang_tests.erl
%% Unit tests for the parser combinators provided by parserlang.erl
%%
%% author: mikeyhc <mikeyhc@atmosia.net>

-module(parserlang_tests).
-include_lib("eunit/include/eunit.hrl").
-export([test_parser/1, test_parser2/1]).

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
