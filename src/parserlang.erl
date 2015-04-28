%% parserlang.erl
%% a small library implementing a subset of the functions provided by
%% the haskell parsec library
%%
%% author: mikeyhc@atmosia.net

-module(parserlang).
-export([% generic parsers
         char/2, case_char/2, case_string/2, oneof/2, string/2,

         % parser combinators
         many/2, many1/2, option/3, either/4,
         both/4, optional/2, between/4, until/2, tryparse/2,
         orparse/3, choice/2, sepby/3, sepby1/3, manyN/3, count/3, manyNtoM/4,

         % type construction
         bin_join/2, bin_concat/1]).

%% converts an uppercase character to its lowercase version, if
%% it is not an uppercase character no change is performed
-spec to_lower(byte()) -> byte().
to_lower(C) when C >= 65 andalso C =< 90 -> C + 32;
to_lower(C) -> C.

%%%%%%%%%%%%%%%%%%%%%%%
%%% Generic Parsers %%%
%%%%%%%%%%%%%%%%%%%%%%%

%% case sensitive character match
-spec char(byte(), <<_:8,_:_*8>>) -> {byte(), binary()}.
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
-spec case_char(byte(), <<_:8,_:_*8>>) -> {byte(), binary()}.
case_char(C, S) when is_binary(S) andalso is_integer(C) ->
    <<H, T/binary>> = S,
    case to_lower(H) == to_lower(C) of
        true -> {H,T};
        false -> throw({parse_error, expected, C})
    end;
case_char(C, _) when not is_integer(C) -> error({badarg, C});
case_char(_, S) -> error({badarg, S}).

%% case insensitive string match
-spec case_string(binary(), binary()) -> {binary(), binary()}.
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
-spec oneof(<<_:8,_:_*8>>, <<_:8,_:_*8>>) -> {byte(), binary()}.
oneof(List, Bin) when is_binary(List) andalso is_binary(Bin) ->
    <<H, T/binary>> = Bin,
    L = binary:bin_to_list(List),
    case lists:member(H, L) of
        true -> {H, T};
        false -> throw({parse_error, expected, "one of \"" ++ L ++ "\""})
    end;
oneof(List, _) when not is_binary(List) -> error({badarg, List});
oneof(_, Bin) -> error({badarg, Bin}).

%% tries to do a case sensitive string compare
-spec string(binary(), binary()) -> {binary(), binary()}.
string(<<>>, T) when is_binary(T) -> {<<>>, T};
string(S, <<>>) when is_binary(S) -> throw({parse_error, expected, S});
string(S, L) when is_binary(S) andalso is_binary(L) ->
    <<SH, ST/binary>> = S,
    try
        {LH, LT} = char(SH, L),
        {RH, RT} = string(ST, LT),
        {<<LH,RH/binary>>, RT}
    catch
        {parse_error, expected, SH} ->
            throw({parse_error, expected, S});
        {parse_error, expected, NT} ->
            throw({parse_error, expected, NT})
    end;
string(S, _) when not is_binary(S) -> error({badarg, S});
string(_, L) when not is_binary(L) -> error({badarg, L}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser Combinators %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%% match 0 of more times using F(A)
-spec many(fun((X) -> {Y, X}), X) -> {[Y], X}.
many(F, A) ->
    try
        {X, Y} = F(A),
        {Acc, T} = many(F, Y),
        {[X|Acc], T}
    catch
        {parse_error, expected, _} -> {[], A};
        error:{badmatch, _} -> {[], A}
    end.

%% match 1 or mote times using F(A)
-spec many1(fun((X) -> {Y, X}), X) -> {[Y], X}.
many1(F, A) ->
    {X, Y} = F(A),
    {Acc, T} = many(F, Y),
    {[X|Acc], T}.

%% tries to match F(A) but if that fails will return {Def, A}
-spec option(Y, fun((X) -> {Y, X}), X) -> {Y, X}.
option(Def, F, A) ->
    try
        F(A)
    catch
        {parse_error, expected, _} -> {Def, A};
        error:{badmatch, _} -> {Def, A}
    end.


%% tries to match one or the other, if both fail the error is thrown
-spec either(fun((X) -> {Y, X}), fun((X) -> {Z, X}), X, string())
      -> {Y | Z, X}.
either(F1, F2, A, Err) ->
    try
        F1(A)
    catch
        {parse_error, expected, _} -> either_(F2, A, Err);
        error:{badmatch, _} -> either_(F2, A, Err)
    end.

-spec either_(fun((X) -> {Z, X}), X, string()) -> {Z, X}.
either_(F, A, Err) ->
    try
        F(A)
    catch
        {parse_error, expected, _} -> throw({parse_error, expected, Err});
        error:{badmatch, _} -> throw({parse_error, expected, Err})
    end.

% tries to match both, if either fail it throws a parse error with the given
% message
-spec both(fun((X) -> {Y, X}), fun((X) -> {Z, X}), X, string()) -> {{Y, Z}, X}.
both(F1, F2, A, Err) ->
    try
        {H1, T1} = F1(A),
        {H2, T2} = F2(T1),
        {{H1, H2}, T2}
    catch
        {parse_error, expected, _} -> throw({parse_error, expected, Err});
        error:{badmatch, _} -> throw({parse_error, expected, Err})
    end.

%% tries to match M:F(A), if that fails returns {<<>>, A}
-spec optional(fun((X) -> {Y, X}), X) -> {Y | binary(), X}.
optional(F, A) -> option(<<>>, F, A).

%% tries to match using H, then will match as much as it can before T matches,
%% the result of which will be passed to F
-spec between(fun((binary()) -> {any(), binary()}),
              fun((binary()) -> {any(), binary()}),
              fun((binary()) -> {any(), binary()}), binary())
      -> {any(), binary()}.
between(H, T, F, A) when is_function(H) andalso is_function(T) andalso
                         is_function(F) ->
    {_, B1} = H(A),
    {B2, Tail} = until(T, B1),
    {F(B2), Tail};
between(H, _, _, _) when not is_function(H) -> error({badarg, H});
between(_, T, _, _) when not is_function(T) -> error({badarg, T});
between(_, _, F, _) when not is_function(F) -> error({badarg, F}).

%% matches until T matches, it is a parse error to reach the end of
%% the binary without encountering T. The matches are returned, as is the
%% tail after T.
%%
%% The function T should return a tuple of the form {match, Tail} if a
%% match was found, otherwise it should return nomatch.
-spec until(fun((binary()) -> {any(), binary()}), binary())
      -> {any(), binary()}.
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
-spec tryparse(fun((binary()) -> {any(), binary()}), binary)
      -> {any(), binary()}.
tryparse(F, A) ->
    try
        F(A)
    catch
        {parse_error, expected, _} -> {<<>>, A};
        error:{badmatch, _} -> {<<>>, A}
    end.

%% orparse attempts to match using the list of parsers, if any of them
%% succeed the result is returned straight away, however if the all fail
%% the given message is produced
-spec orparse([fun((binary()) -> {any, binary})], binary(), any())
      -> {any(), binary()}.
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
-spec choice([fun((binary()) -> {any(), binary()})], any())
      -> fun((binary()) -> {any(), binary()}).
choice(FuncList, Error) -> fun(X) -> choice_(X, FuncList, Error) end.

-spec choice_(binary(), [fun((binary()) -> {any(), binary()})], any())
      -> {any(), binary()}.
choice_(_, [], Err) -> throw({parse_error, expected, Err});
choice_(B, [H|T], Err) ->
    try
        H(B)
    catch
        {parse_error, expected, _} -> choice_(B, T, Err);
        error:{badmatch, _} -> choice_(B, T, Err)
    end.

%% takes a function to match content and another to match seperator and
%% text to match, will return any number of text interspersed with
%% the seperator
-spec sepby(fun((binary()) -> {X, binary()}),
            fun((binary()) -> {any(), binary()}),
            binary()) -> {[X], binary()}.
sepby(Content, Sep, Text) ->
    try
        {H1, T1} = Content(Text),
        try
            {_, T2} = Sep(T1),
            {H3, T3} = sepby(Content, Sep, T2),
            {[H1|H3], T3}
        catch
            {parse_error, expected, _} -> {[H1], T1};
            error:{badmatch, _} -> {[H1], T1}
        end
    catch
        {parse_error, expected, _} -> {[], Text};
        error:{badmatch, _} -> {[], Text}
    end.

%% same as sepby/3 but matches at least once
-spec sepby1(fun((binary()) -> {X, binary()}),
             fun((binary()) -> {any(), binary()}),
             binary()) -> {[X, ...], binary()}.
sepby1(Content, Sep, Text) ->
    {H1, T1} = Content(Text),
    {_, T2} = Sep(T1),
    {H3, T3} = sepby(Content, Sep, T2),
    {[H1|H3], T3}.

%% matches a parser at least N times
-spec manyN(non_neg_integer(), fun((binary()) -> {X, binary()}),
            binary()) -> {[X], binary()}.
manyN(N, P, X) when is_integer(N) andalso is_function(P) andalso
                    is_binary(X) ->
    {H1, T1} = count(N, P, X),
    {H2, T2} = many(P, T1),
    {H1 ++ H2, T2};
manyN(N, _, _) when not is_integer(N) -> error({badarg, N});
manyN(_, P, _) when not is_function(P) -> error({badarg, P});
manyN(_, _, X) when not is_binary(X) -> error({badarg, X}).

%% matches a parser exactly N times
-spec count(non_neg_integer(), fun((binary()) -> {X, binary()}),
            binary()) -> {[X], binary()}.
count(N, _, X) when N =< 0 -> {[], X};
count(N, P, X) when is_integer(N) andalso is_function(P) andalso
                    is_binary(X) ->
    {H1, T1} = P(X),
    {H2, T2} = count(N-1, P, T1),
    {[H1|H2], T2};
count(N, _, _) when not is_integer(N) -> error({badarg, N});
count(_, P, _) when not is_function(P) -> error({badarg, P});
count(_, _, X) when not is_binary(X) -> error({badarg, X}).

%% match a parser at least N times, but no more than M times
-spec manyNtoM(non_neg_integer(), non_neg_integer(),
               fun((binary()) -> {X, binary()}),
               binary()) -> {[X], binary()}.
manyNtoM(N, _, _, X) when N < 0 -> {[], X};
manyNtoM(N, M, _, X) when N > M -> {[], X};
manyNtoM(N, M, P, X) when N == M -> count(N, P, X);
manyNtoM(N, M, P, X) when N == 0 ->
    MkFunc = fun(Y) -> fun(Z) -> count(Y, P, Z) end end,
    List = lists:map(MkFunc, lists:seq(M, 1, -1)),
    orparse(List, X, "manyNtoM failed");
manyNtoM(N, M, P, X) when is_integer(N) andalso is_integer(M) ->
    {H1, T1} = count(N, P, X),
    {H2, T2} = manyNtoM(0, M-N, P, T1),
    {H1 ++ H2, T2}.


%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type Construction %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

%% joins characters and binarys
-spec bin_join(byte() | binary(), byte() | binary()) -> binary().
bin_join(X, Y) when is_binary(X) andalso is_binary(Y) ->
    <<X/binary, Y/binary>>;
bin_join(X, Y) when is_integer(X) andalso is_binary(Y) ->
    <<X, Y/binary>>;
bin_join(X, Y) when is_binary(X) andalso is_integer(Y) ->
    <<X/binary, Y>>;
bin_join(X, Y) -> <<X, Y>>.

%% joins a list of characters and binaries
-spec bin_concat([byte() | binary()]) -> binary().
bin_concat(L) -> lists:foldl(fun(X, Acc) -> bin_join(Acc, X) end, <<>>, L).
