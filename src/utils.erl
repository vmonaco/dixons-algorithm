%% Author: vinnie
%% Created: Dec 17, 2012
%% Description: TODO: Add description to utils
-module(utils).

%%
%% Exported Functions
%%
-export([floor/1, ceiling/1, gcd/2, sieve/1, isqrt/1, ipow/2, ipow/3, 
         highestPow/2]).

%%
%% API Functions
%%

% Floor function
floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

% Ceiling function
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

%% Euclid's algorthim for computing GCD
gcd(A, 0) ->
    A;
gcd(A, B) ->
    gcd(B, A rem B).

%% Sieve of Eratosthenes, generates primes numbers
sieve([]) ->
    [];
sieve([H|T]) ->
    List = lists:filter(fun(N) -> N rem H /= 0 end, T),
    [H|sieve(List)];
sieve(N) ->
    sieve(lists:seq(2,N)).

%% Integer square root, to avoid floats
isqrt(Num) when Num =< 0 ->
     0;
isqrt(Num) ->
    isqrt(Num, 1, (1 + Num) div 2).

isqrt(Num, M, N) when abs(M - N) =< 1 ->
    if  N * N =< Num -> N;
        true         -> N - 1
    end;
isqrt(Num, _, N) ->
    isqrt(Num, N, (N + Num div N) div 2).

%% Integer power, to avoid floats
ipow(A, 1) ->
    A;
ipow(A, 2) ->
    A*A;
ipow(A, B) ->
    B1 = B div 2,
    B2 = B - B1,
    %% B2 = B1 or B1+1
    P = ipow(A, B1),
    case B2 of
        B1 -> (P*P);
        _  -> (P*P*A)
    end.

%% Same as above with rem taken
ipow(A, 1, M) ->
    A rem M;
ipow(A, 2, M) ->
    A*A rem M;
ipow(A, B, M) ->
    B1 = B div 2,
    B2 = B - B1,
    %% B2 = B1 or B1+1
    P = ipow(A, B1, M),
    case B2 of
        B1 -> (P*P) rem M;
        _  -> (P*P*A) rem M
    end.

% Test for divisibility
highestPow(A, B) ->
    highestPow(A, B, 0).
highestPow(0, _, P) ->
    P;
highestPow(A, B, P) ->
    case A rem B of
        0 -> highestPow(A div B, B, P+1);
        _ -> P
    end.