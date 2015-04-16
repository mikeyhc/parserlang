%% parserlang.erl
%% a small library implementing a subset of the functions provided by
%% the haskell parsec library
%%
%% author: mikeyhc@atmosia.net

-module(parserlang).
-export([% generic parsers
         char/2, case_char/2, case_string/2, oneof/2,

         % parser combinators
         many/3, many1/3, option/4, either/6, both/6, optional/3, between/5,
         until/2, tryparse/3, orparse/3, choice/2,

         % type construction
         bin_join/2, bin_concat/1]).

%% converts an uppercase character to its lowercase version, if
%% it is not an uppercase character no change is performed
to_lower(C) when C >= 65 andalso C =< 90 -> C + 32;
to_lower(C) -> C.

%%%%%%%%%%%%%%%%%%%%%%%
%%% Generic Parsers %%%
%%%%%%%%%%%%%%%%%%%%%%%

%% case sensitive character match
char(C, S) when is_binary(S) andalso is_integer(C) ->
    try
        <<C, T/binary>> = S,
        {C, T}
    catch
        error:{badmatch, _} -> throw({parse_error, expected, C})
    end;
char(_, S) when not is_binary(S) -> error({badarg, S});
char(C, _) -> error({badarg, C}).

%% case insensitive character match
case_char(C, S) when is_binary(S) andalso is_integer(C) ->
    <<H, T/binary>> = S,
    case to_lower(H) == to_lower(C) of
        true -> {H,T};
        false -> throw({parse_error, expected, C})
    end;
case_char(C, _) when not is_integer(C) -> error({badarg, C});
case_char(_, S) -> error({badarg, S}).

%% case insensitive string match
case_string(<<>>, L) when is_binary(L) -> {<<>>, L};
case_string(S, <<>>) when is_binary(S) ->
    throw({parse_error, expected, S});
case_string(S, L) when is_binary(L) andalso is_binary(S) ->
    <<SH, ST/binary>> = S,
    try
        {LH, LT} = case_char(SH, L),
        {RH, RT} = case_string(ST, LT),
        {<<LH,RH/binary>>, RT}
    catch
        {parse_error, expected, SH} ->
            throw({parse_error, expected, S});
        {parse_error, expected, NT} ->
            throw({parse_error, expected, NT})
    end;
case_string(S, _) when not is_binary(S) -> error({badarg, S});
case_string(_, L) -> error({badarg, L}).

%% tries to match the head of Bin one of List, throws a parse_error if
%% it cannot
oneof(List, Bin) when is_binary(List) andalso is_binary(Bin) ->
    <<H, T/binary>> = Bin,
    L = binary:bin_to_list(List),
    case lists:member(H, L) of
        true -> {H, T};
        false -> throw({parse_error, expected, "one of \"" ++ L ++ "\""})
    end;
oneof(List, _) when not is_binary(List) -> error({badarg, List});
oneof(_, Bin) -> error({badarg, Bin}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser Combinators %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%% match 0 or more times using M:F(A)
many(M, F, A) ->
    try
        {X, Y} = apply_many(M, F, A),
        {Acc, T} = many(M, F, Y),
        {bin_join(X, Acc), T}
    catch
        {parse_error, expected, _} -> {<<>>, A};
        error:{badmatch, <<>>}     -> {<<>>, A}
    end.

%% match 1 or more times using apply_many
many1(M, F, A) ->
    {X, Y} = apply_many(M, F, A),
    {Acc, T} = many(M, F, Y),
    {<<X, Acc/binary>>, T}.

apply_many(M, F, A) when is_list(A) -> apply(M, F, A);
apply_many(M, F, A) -> M:F(A).

%% tries to match M:F(A), but if that fails will return {Def, A}
option(Def, M, F, A) ->
    try
        apply_many(M, F, A)
    catch
        {parse_error, expected, _} -> {Def, A}
    end.

%% tries to match one or the other, if both fail the error is thrown
either(M1, F1, M2, F2, A, Err) ->
    try
        M1:F1(A)
    catch
        {parse_error, expected, _} -> either_(M2, F2, A, Err);
        error:{badmatch, _} -> either_(M2, F2, A, Err)
    end.

either_(M, F, A, Err) ->
    try
        M:F(A)
    catch
        {parse_error, expected, _} -> throw({parse_error, expected, Err});
        error:{badmatch, _} -> throw({parse_error, expected, Err})
    end.

% tries to match both, if either fail it throws a parse error with the given
% message
both(M1, F1, M2, F2, A, Err) ->
    try
        {H1, T1} = apply_many(M1, F1, A),
        {H2, T2} = apply_many(M2, F2, T1),
        {bin_join(H1, H2), T2}
    catch
        {parse_error, expected, _} -> throw({parse_error, expected, Err});
        error:{badmatch, _} -> throw({parse_error, expected, Err})
    end.

%% tries to match M:F(A), if that fails returns {<<>>, A}
optional(M, F, A) -> option(<<>>, M, F, A).

%% tries to match using H, then will match as much as it can before T matches,
%% the result of which will be passed to M:F
between(H, T, M, F, A) when is_function(H) andalso is_function(T) andalso
                            is_binary(A) ->
    {_, B1} = H(A),
    {B2, Tail} = until(T, B1),
    {M:F(B2), Tail};
between(H, _, _, _, _) when not is_function(H) -> error({badarg, H});
between(_, T, _, _, _) when not is_function(T) -> error({badarg, T});
between(_, _, _, _, A) when not is_binary(A) -> error({badarg, A}).

%% matches until T matches, it is a parse error to reach the end of
%% the binary without encountering T. The matches are returned, as is the
%% tail after T.
%%
%% The function T should return a tuple of the form {match, Tail} if a
%% match was found, otherwise it should return nomatch.
until(T, B) when is_function(T) andalso is_binary(B) ->
    case T(B) of
        {match, Match} -> {<<>>, Match};
        nomatch ->
            <<H, Temp/binary>> = B,
            {Rest, Tail} = until(T, Temp),
            {<<H, Rest/binary>>, Tail}
    end;
until(T, _) when not is_function(T) -> error({badarg, T});
until(_, B) when not is_binary(B) -> error({badarg, B}).

%% try attempts to match using the given parser, if it fails the empty
%% binary will be returned as the head and the tail will be the binary
%% that was originally passed in
tryparse(M, F, A) ->
    try
        apply_many(M, F, A)
    catch
        {parse_error, expected, _} -> {<<>>, A};
        error:{badmatch, _} -> {<<>>, A}
    end.

%% orparse attempts to match using the list of parsers, if any of them
%% succeed the result is returned straight away, however if the all fail
%% the given message is produced
orparse([], _, Msg) -> throw({parse_error, expected, Msg});
orparse([H|T], A, Msg) when is_function(H) ->
    try
        H(A)
    catch
        {parse_error, expected, _} -> orparse(T, A, Msg);
        error:{badmatch, _} -> orparse(T, A, Msg)
    end;
orparse([{M,F}|T], A, Msg) ->
    try
        M:F(A)
    catch
        {parse_error, expected, _} -> orparse(T, A, Msg);
        error:{badmatch, _} -> orparse(T, A, Msg)
    end;
orparse(Arg, _, _) -> error({badarg, Arg}).

%% returns a function that that takes input and attempts to match it with
%% any of the functions passed to it at creation
choice(FuncList, Error) -> fun(X) -> choice_(X, FuncList, Error) end.

choice_(_, [], Err) -> throw({parse_error, expected, Err});
choice_(B, [H|T], Err) ->
    try
        H(B)
    catch
        {parse_error, expected, _} -> choice_(B, T, Err);
        error:{badmatch, _} -> choice_(B, T, Err)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type Construction %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

%% joins characters and binarys
bin_join(X, Y) when is_binary(X) andalso is_binary(Y) ->
    <<X/binary, Y/binary>>;
bin_join(X, Y) when is_integer(X) andalso is_binary(Y) ->
    <<X, Y/binary>>;
bin_join(X, Y) when is_binary(X) andalso is_integer(Y) ->
    <<X/binary, Y>>;
bin_join(X, Y) -> <<X, Y>>.

%% joins a list of characters and binaries
bin_concat(L) -> lists:foldl(fun(X, Acc) -> bin_join(Acc, X) end, <<>>, L).
