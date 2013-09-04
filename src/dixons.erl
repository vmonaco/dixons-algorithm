%% Author: vinnie
%% Created: Dec 17, 2012
%% Description: TODO: Add description to dixons
-module(dixons).

%%
%% Exported Functions
%%
-export([start/4, test/0, receive_bSmooth/4, search_iterative/4, search_random/3]).

%%
%% API Functions
%%

% Start Dixon's Algorithm
start(N, B_Bound, N_Relations, N_Procs) ->
    B = utils:sieve(B_Bound),
    register(receive_bSmooth, spawn(dixons, receive_bSmooth, [N_Procs, 0, [], N_Relations])),
    spawn_searches(N_Procs, utils:ceiling(math:sqrt(N)), utils:ceiling((N - math:sqrt(N))/N_Procs), N, B).

% Test it on a the Wikipedia example
test() ->
    start(84923, 10, 20, 4).

%%
%% Local Functions
%%

% Predicate to test for B-smoothness
bSmooth(A, B) ->
    bSmooth(A, B, []).
bSmooth(1, B, Factors) ->
    {smooth, lists:reverse(Factors)};
bSmooth(Y, [], Factors) ->
    {not_smooth, lists:reverse(Factors)};
bSmooth(Y, [P|B], Factors) ->
    F = utils:highestPow(Y, P),
    case F of
        0 -> bSmooth(Y, B, [F|Factors]);
        _ -> bSmooth(Y div utils:ipow(P, F), B, [F|Factors])
    end.

% Recieve the relations coming in from other processes
receive_bSmooth(0, N_Relations, Relations, Target) ->
    io:format("Search ended prematurely, unable to meet target ~p ~p.~n", [N_Relations, Target]);
receive_bSmooth(_, N_Relations, Relations, Target) when N_Relations =:= Target ->
    io:format("Search ended successfully.~n", []);
receive_bSmooth(N_Searches, N_Relations, Relations, Target) ->
    receive
        {PID, finished} ->
            receive_bSmooth(N_Searches - 1, N_Relations, Relations, Target);
        {PID, smooth, V, R} ->
            io:format("From ~p Received ~p: ~w~n", [PID, R, V]),
            receive_bSmooth(N_Searches, N_Relations + 1, [V,Relations], Target)
    end.

% Search randomly within the interval
search_random(0, _, _) ->
    receive_bSmooth ! {self(), finished},
    io:format("seach_random ~p finished~n", [self()]);
search_random(Max_Iter, {L_Bound, U_Bound}, Base) ->
    R = random:uniform(U_Bound - L_Bound) + L_Bound,
    {Smoothness, Factorization} = bSmooth(R, Base),
    case Smoothness of
        not_smooth ->
            pass;
        smooth ->
            receive_bSmooth ! {self(), Factorization, R}
    end,
    search_random(Max_Iter - 1, {L_Bound, U_Bound}, Base).

% Search iteratively through the interval
search_iterative(L_Bound, U_Bound, _, _) when L_Bound =:= U_Bound ->
    receive_bSmooth ! {self(), finished},
    io:format("seach_iterative ~p finished~n", [self()]);
search_iterative(L_Bound, U_Bound, N, Base) ->
    {Smoothness, Factorization} = bSmooth(utils:ipow(L_Bound, 2, N), Base),
    case Smoothness of
        not_smooth ->
            pass;
        smooth ->
            receive_bSmooth ! {self(), smooth, Factorization, L_Bound}
    end,
    search_iterative(L_Bound + 1, U_Bound, N, Base).

% Spawn searches over evenly-spaced intervals
spawn_searches(0, _, _, _, _) ->
    ok;
spawn_searches(Num, L_Bound, Span, N, B) ->
    U_Bound = L_Bound + Span,
    spawn(dixons, search_iterative, [L_Bound, U_Bound, N, B]),
    spawn_searches(Num-1, U_Bound, Span, N, B).