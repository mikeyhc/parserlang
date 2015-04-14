%% parserlang.erl
%% a small library implementing a subset of the functions provided by
%% the haskell parsec library
%%
%% author: mikeyhc@atmosia.net

-module(parserlang).
-export([% parser combinators
         many/3, many1/3, option/4, either/6, both/6, optional/3, between/5,
         until/2,

         % type construction
         bin_join/2, bin_concat/1]).

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

%% match 1 or more times using M:F(A)
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

%% tries to match H, then will match as much as it can before encountering
%% T, the result of which will be passed to M:F
between(H, T, M, F, A) when is_binary(H) andalso is_binary(T) andalso
                            is_binary(A) ->
    B1 = try
             Size = byte_size(H),
             <<H:Size/binary, B/binary>> = A,
             B
         catch
             error:{badmatch, _} -> throw({parse_error, expected, H})
         end,
    {B2, Tail} = until(T, B1),
    {M:F(B2), Tail};
between(H, _, _, _, _) when not is_binary(H) -> error({badarg, H});
between(_, T, _, _, _) when not is_binary(T) -> error({badarg, T});
between(_, _, _, _, A) when not is_binary(A) -> error({badarg, A}).

%% matches until T is encountered, it is a parse error to reach the end of
%% the binary without encountering T. The matches are returned, as is the
%% tail after T.
until(T, B) when is_binary(T) andalso is_binary(B) ->
    case binary:match(B, T) of
        nomatch -> throw({parse_error, expected, T});
        {S, E} ->
            Body = binary:part(B, {0, S}),
            Tail = binary:part(B, {S+E, byte_size(B) - (S+E)}),
            {Body, Tail}
    end;
until(T, _) when not is_binary(T) -> error({badarg, T});
until(_, B) when not is_binary(B) -> error({badarg, B}).

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

