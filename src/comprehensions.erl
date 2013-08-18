-module(comprehensions).

-export([divible_by_seq/3, squared_ints/1,
         intersection/2, symmetric_difference/2]).

%% create a set of integers between 1 and 10 that are divisible
%% by three (e.g., [3,6,9]).
divible_by_seq(Lb, Ub, Div) ->
    [X || X <- lists:seq(Lb, Ub), X rem Div == 0].

%% remove all non-integers from a polymorphic list
squared_ints(List) -> [X * X || X <- List, is_integer(X)].

%% return a new list that is the intersection
%% of the two lists
intersection(LhList, RhList) ->
    [X || X <- LhList, Y <- RhList, X == Y].

%% return a new list that is the symmetric difference of the two
%% lists.
%% Using [1,2,3,4,5] and [4,5,6,7,8] should return
%% [1,2,3,6,7,8].
symmetric_difference(LhList, RhList) ->
    Intersection = intersection(LhList, RhList),
    (LhList -- Intersection) ++ (RhList -- Intersection).

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

divible_by_seq_test_() ->
    [
     ?_assertEqual([3, 6, 9],
                   divible_by_seq(1, 10, 3)),
     ?_assertEqual([], divible_by_seq(1, 99, 100)),
     ?_assertEqual(hof:even_seq(1, 100),
                   divible_by_seq(1, 100, 2))
    ].

squared_ints_test_() ->
    [
     ?_assertEqual(
        lists:map(fun (X) -> X * X end, lists:seq(1, 10)),
        squared_ints([1, 2, 3, "lol", 4, null, 5, 6, 7, 8, 9, 10])),
     ?_assertEqual([], squared_ints([])),
     ?_assertEqual([], squared_ints([lol, null, "str"]))
    ].

inersec_test_() ->
    [
     ?_assertEqual([4,5], intersection([1,2,3,4,5], [4,5,6,7,8])),
     ?_assertEqual([], intersection([], [])),
     ?_assertEqual([], intersection([1, 2, 3], [4, 5, 6])),
     ?_assertEqual([lol], intersection([1, lol, 3], [lol, fpm, 6]))
    ].

symmetric_difference_test_() ->
    [
     ?_assertEqual([1,2,3,6,7,8],
                  symmetric_difference([1,2,3,4,5], [4,5,6,7,8])),
     ?_assertEqual([],
                  symmetric_difference([1, 2, 3, 4], [1, 2, 3, 4])),
     ?_assertEqual([], symmetric_difference([], [])),
     ?_assertEqual([1, 2, 3, 4], symmetric_difference([1, 2], [3, 4]))
    ].

-endif.
