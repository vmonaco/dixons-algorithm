%% Author: vinnie
%% Created: Dec 17, 2012
%% Description: TODO: Add description to linalg
-module(linalg).

%%
%% Exported Functions
%%
-export([test/0]).

%%
%% API Functions
%%



%%
%% Local Functions
%%
argmax([H|T], Idx, Max, Max_Idx) when H > Max ->
    argmax(T, Idx+1, Max, Idx);
argmax([H|T], Idx, Max, Max_Idx) when H =< Max ->
    argmax(T, Idx+1, Max, Max_Idx);
argmax([], _, Max, Max_Idx) ->
    {Max, Max_Idx}.

set(A, I, J, X) ->
    erlang:setelement(I, A, erlang:setelement(J, erlang:element(I, A), X)).

get(A, I, J) ->
    erlang:element(J, erlang:element(I, A)).
  
% A[i, j] := A[i, j] - A[k, j] * (A[i, k] / A[k, k])
reduce(A, K, I, J) ->
    X = get(A, I, J) - (get(A, K, J)*(get(A, I, K)/get(A, K, K))),
    io:format("~p\n", [X]),
    set(A, I, J, X).

for_j(A, K, I, J, N) when J =< N ->
    New_A = reduce(A, K, I, J),
    io:format("~p\n", [A]),
    for_j(New_A, K, I, J+1, N);
for_j(A, K, I, J, N) ->
    A.

for_i(A, K, I, M, N) when I =< M ->
    New_A = for_j(A, K, I, K+1, N),
    AA = set(New_A, I, K, 0),
    for_i(AA, K, I+1, M, N);
for_i(A, K, I, M, N) ->
    A.

for_k(A, K, M, N) when K =< M ->
    New_A = for_i(A, K, K+1, M, N),
    for_k(New_A, K+1, M, N);
for_k(A, K, M, N) ->
    A.

accum_inner(X, Sol, Ans, A, Inner, M, N) when X < Sol ->
    io:format("inner ~p, ~p, ~p, ~p~n", [X, Sol, Ans, Inner]),
    Accum = lists:nth(X, Ans)*get(A, M-Sol+1, N-X),
    accum_inner(X+1, Sol, Ans, A, Inner + Accum, M, N); 
accum_inner(X, Sol, Ans, A, Inner, M, N) ->
    Inner.

for_sol(A, Sol, M, N, Solution) when Sol =:= 1 ->
    S = get(A, M, N)/get(A, M, N-1),
%%     io:format("** ~p, ~p, ~p, ~p~n", [A, M, N, S]),
    for_sol(A, Sol+1, M, N, Solution ++ [S]);
for_sol(A, Sol, M, N, Solution) when Sol =< M ->
    Inner = accum_inner(1, Sol, Solution, A, 0, M, N),
    io:format("~p, ~p, ~p~n", [Sol, Solution, Inner]),
    S = (get(A, M-Sol+1, N)-Inner)/get(A, M-Sol+1, N-Sol),
    for_sol(A, Sol+1, M, N, Solution ++ [S]);
for_sol(A, Sol, M, N, Solution) ->
    Solution.

solve(A, M, N) ->
    lists:reverse(for_sol(A, 1, M, N, [])).
    
gaussian_elimination(A) ->
    M = erlang:size(A),
    N = erlang:size(erlang:element(1, A)),
    GA = for_k(A, 1, M, N),
    solve(GA, M, N).

test() ->
    A = {{2, 1, -1, 8}, {-3, -1, 2, -11}, {-2, 1, 2, -3}},
    gaussian_elimination(A).