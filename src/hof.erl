-module(hof).

-export([print_ints/2,
         smaller_or_equal/2,
         even_seq/2,
         print_even/2,
         list_concat/1,
         sum/1]).

print_ints(Lb, Ub) ->
    lists:foreach(fun (X) -> io:format("~b~n", [X]) end,
                  lists:seq(Lb, Ub)).

smaller_or_equal(Seq, Int) ->
    lists:filter(
      fun (X) -> X =< Int end,
      Seq).

even_seq(Lb, Ub) ->
    lists:filter(
      fun (X) -> X rem 2 == 0 end,
      lists:seq(Lb, Ub)).

print_even(Lb, Ub) ->
    lists:foreach(
      fun (X) -> io:format("~b~n", [X]) end,
      even_seq(Lb, Ub)).

list_concat(ListOfLists) ->
    lists:foldr(
      fun (List, Acc) -> List ++ Acc end, [],
      ListOfLists).

sum(List) -> lists:foldl(fun (X, Sum) -> X + Sum end, 0, List).

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

smaller_or_equal_test_() ->
    [
     ?_assertEqual(lists:seq(1, 10),
                   smaller_or_equal(lists:seq(1, 10), 10)),
     ?_assertEqual([1, 2, 3, 4],
                   smaller_or_equal(lists:seq(1, 10), 4)),
     ?_assertEqual([1, 3, 5],
                   smaller_or_equal([10, 23, 1, 393, 3, 2020, 5], 5))
    ].

even_seq_test_() ->
    [
     ?_assertEqual([2, 4, 6, 8, 10],
                   even_seq(2, 10)),
     ?_assertEqual([12, 14, 16, 18, 20],
                   even_seq(12, 20)),
     ?_assertEqual([0], even_seq(0, 0))

    ].

list_concat_test_() ->
    [
     ?_assertEqual("Hey, folks",
                   list_concat(["Hey, ", "fo", "lk", "s"])),
     ?_assertEqual([1, 2, 3, 4],
                   list_concat([[1], [2, 3], [4]])),
     ?_assertEqual([], list_concat([]))
    ].

sum_test_() ->
    [
     ?_assertEqual(0, sum([0])),
     ?_assertEqual(lists:sum(lists:seq(10, 100)),
                   sum(lists:seq(10, 100))),
     ?_assertEqual(lists:sum(lists:seq(-10, 0)),
                   sum(lists:seq(-10, 0)))
    ].

-endif.
