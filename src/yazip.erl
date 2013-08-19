-module(yazip).

-export([zip/2, zip_rec/2, zipwith/3, zipwith_rec/3]).

%% Isn't it simplier than another one below?
zip_rec([], _) -> [];
zip_rec(_, []) -> [];
zip_rec([LH|LTail], [RH|RTail]) -> [{LH, RH}|zip_rec(LTail, RTail)].

zipwith_rec(_, [], _) -> []; 
zipwith_rec(_, _, []) -> [];
zipwith_rec(F, [LH|LTail], [RH|RTail]) -> [F(LH, RH)|zipwith_rec(F, LTail, RTail)].

zip(LhList, RhList) -> 
    zipwith(fun (X, Y) -> {X, Y} end, LhList, RhList).

zipwith(F, LhList, RhList) ->
    [F(X, Y) || I <- lists:seq(1, min(length(LhList), length(RhList))), 
                X <- [lists:nth(I, LhList)], Y <- [lists:nth(I, RhList)]].

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

zip_test_() ->
    [
     ?_assertEqual([{1,3}, {2,4}], zip([1, 2], [3, 4, 5])),
     ?_assertEqual([], zip([], [1, 2, 3, 4])),
     ?_assertEqual([], zip([1, 2, 3, 4], []))
    ].

zip_rec_test_() ->
    [
     ?_assertEqual([{1,3}, {2,4}], zip_rec([1, 2], [3, 4, 5])),
     ?_assertEqual([], zip_rec([], [1, 2, 3, 4])),
     ?_assertEqual([], zip_rec([1, 2, 3, 4], []))
    ].

add(X, Y) -> X + Y.

zipwith_test_() ->
    [
     ?_assertEqual([4, 6], zipwith(fun add/2, [1, 2], [3, 4, 5])),
     ?_assertEqual([], zipwith(fun add/2, [], [1, 2, 3, 4])),
     ?_assertEqual([], zipwith(fun add/2, [1, 2, 3, 4], [])),
     ?_assertEqual([], zipwith(fun add/2, [], []))
    ].

zipwith_rec_test_() ->
    [
     ?_assertEqual([4, 6], zipwith_rec(fun add/2, [1, 2], [3, 4, 5])),
     ?_assertEqual([], zipwith_rec(fun add/2, [], [1, 2, 3, 4])),
     ?_assertEqual([], zipwith_rec(fun add/2, [1, 2, 3, 4], [])),
     ?_assertEqual([], zipwith_rec(fun add/2, [], []))
    ].

-endif.
