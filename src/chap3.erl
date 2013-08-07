-module(chap3).
%% TODO: add export functions
-compile(export_all).

%% 3.1 Function sum/1
%% N - upperbound
%% return the sum of all the integers between 1 and N
sum(N) when N > 1 ->
    N + sum(N - 1);
sum(1) ->
    1.

%% 3.1 Function sum/2
%% N - lowerbound
%% M - upperbound
%% return the sum of the interval between N and M
%% throw badarg when N > M
sum(N, M) when N < M ->
    M + sum(N, M - 1);
sum(N, N) ->
    N;
sum(N, M) when N > M ->
    throw({badarg, M}).

%% 3.2 Function create/1
%% N - value of the last element in the list
%% return [1, 2, ..., N - 1, N] sequence
create(N) -> create_acc(N, []).
%% acc function for create/1
create_acc(0, List) -> List;
create_acc(N, List) -> create_acc(N - 1, [N|List]).

%% 3.2 Function reverse_create/1
%% N - value of the first element in the list
%% return [N, N - 1, ..., 2, 1] sequence
reverse_create(0) ->
    [];
reverse_create(N) ->
    [N|reverse_create(N - 1)].

%% 3.3 print_ints - print to stdout number in format "Number:~p~n"
%% N - max number in sequnce
print_ints(N) when N > 0 ->
    print_ints(N - 1),
    io:format("Number:~p~n", [N]);
%% how to avoid this clause?
print_ints(0) -> 0.

%% 3.3 print_even - print to stdout number in format "Number:~p~n"
%% only if it's even
%% N - max number in sequnce
print_even(N) when N rem 2 == 0, N > 0 ->
    print_even(N - 1),
    io:format("Number:~p~n", [N]);
print_even(N) when N > 0 ->
    print_even(N - 1);
%% how to avoid this clause?
print_even(0) -> 0.

%% 3.5 Manipulating Lists
%% filter - return all integers smaller than or equal to given integer
%% List - list of  integers
%% N - upperbound
filter(List, N) ->
    case List of
        [] -> [];
        [H|T] when H =< N ->
            [H|filter(T, N)];
        [_|T] -> filter(T, N)
    end.

%% reverse - return reversed version of given list
%% List - list that should be reversed
reverse(List) ->
    reverse_acc(List, []).

reverse_acc([H|T], Acc) ->
    reverse_acc(T, [H|Acc]);
reverse_acc([], Acc) -> Acc.


%% concatenate
%% concatenate all given lists and return it
%% Lists - list of lists
concatenate(Lists) ->
    case Lists of
        [] -> [];
        [H|T] -> concatenate(H, concatenate(T))
    end.

%% concatenate two give lists
%% TODO: should we append small list to bigger one?
concatenate([], Dst) ->
    Dst;
concatenate(Src, []) ->
    Src;
concatenate([H|T], Dst) ->
    [H|concatenate(T, Dst)].

%% flatten
%% return a flat list of given list of lists
%% Lists - list of lists
flatten(Lists) ->
    case Lists of
        [] -> [];
        [H|T] -> concatenate(flatten(H), flatten(T));
        L -> [L]
    end.

%% 3.6 Sorting Lists
