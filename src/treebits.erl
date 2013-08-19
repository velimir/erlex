-module(treebits).

%% TODO: add export
-compile([export_all]).
-export([treeToList/1, deserialize/1, listToTree/1, treeToBin/1]).

-include("dbg.hrl").

treeToList({leaf, N}) ->
    [2, N];
treeToList({node, T1, T2}) ->
    TTL1 = treeToList(T1),
    [Size1|_] = TTL1,
    TTL2 = treeToList(T2),
    [Size2|List2] = TTL2,
    [Size1 + Size2|TTL1 ++ List2].

deserialize([_|Ls]) ->
    listToTree(Ls).

listToTree([2, N]) ->
    {leaf, N};
listToTree([N]) ->
    {leaf, N};
listToTree([M|Rest] = _Code) ->
    {Code1, Code2} = lists:split(M - 1, Rest),
    {node,
     listToTree(Code1),
     listToTree(Code2)
    }.

treeToBin({leaf, N}) ->
    TermBin = term_to_binary(N),
    ContentSize = byte_size(TermBin),
    Size = byte_size(<<ContentSize>>),
    <<(Size + ContentSize), TermBin/binary>>;
treeToBin({node, LTree, RTree}) ->
    LBinTree = treeToBin(LTree),
    <<LBinSize/integer, _/binary>> = LBinTree,
    RBT = treeToBin(RTree),
    <<RBinSize/integer, RBinTree/binary>> = RBT,
    <<(LBinSize + RBinSize), LBinTree/binary, RBinTree/binary>>.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

treeToList_test_() ->
    [
     ?_assertEqual([2, dog], treeToList({leaf, dog})),
     ?_assertEqual([4, 2, dog, emu],
                   treeToList({node, {leaf, dog}, {leaf, emu}})),
     ?_assertEqual([6, 2, cat, 2, dog, emu],
                   treeToList({node, 
                               {leaf, cat}, 
                               {node, {leaf, dog}, {leaf, emu}}})),
     ?_assertEqual([8, 6, 2, cat, 2, dog, emu, fish],
                   treeToList({node, 
                               {node, {leaf, cat}, 
                                {node, {leaf, dog}, {leaf, emu}}},
                               {leaf, fish}}))
    ].

deserialize_test_() ->
    [
     ?_assertEqual({leaf, dog}, deserialize([2, dog])),
     ?_assertEqual({node, {leaf, dog}, {leaf, emu}},
                   deserialize([4, 2, dog, emu])),
     ?_assertEqual({node, 
                    {leaf, cat}, 
                    {node, {leaf, dog}, {leaf, emu}}},
                   deserialize([6, 2, cat, 2, dog, emu])),
     ?_assertEqual({node, 
                    {node, {leaf, cat}, 
                     {node, {leaf, dog}, {leaf, emu}}},
                    {leaf, fish}},
                   deserialize([8, 6, 2, cat, 2, dog, emu, fish]))
    ].

-endif.
