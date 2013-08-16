-module(btree).

-export([new/1, insert/2, inorder_fold/3, max/1, sum/1, is_ordered/1]).

-include("btree.hrl").

new(Value) ->
    #node{value = Value}.

insert(Value, #node{value = V, right = Right} = Node) when Value >= V ->
    case Right of
        null ->
            Node#node{right = #node{value = Value}};
        _ ->
            Node#node{right = insert(Value, Right)}
    end;
insert(Value, #node{value = V, left = Left} = Node) when Value < V ->
    case Left of
        null ->
            Node#node{left = #node{value = Value}};
        _ ->
            Node#node{left = insert(Value, Left)}
    end.

inorder_fold(F, Acc, Node) when is_record(Node, node) ->
    visit(F, Acc, Node).

visit(F, Acc, #node{left = null, right = null} = Node) ->
    F(Node, Acc);
visit(F, Acc, #node{left = null, right = Right} = Node) ->
    visit(F, F(Node, Acc), Right);
visit(F, Acc, #node{left = Left, right = null} = Node) ->
    F(Node, visit(F, Acc, Left));
visit(F, Acc, #node{left = Left, right = Right} = Node) ->
    visit(F, F(Node, visit(F, Acc, Left)), Right).

max(#node{value = Value}, null) -> Value;
max(#node{value = Value}, Acc) when Value >= Acc -> Value;
max(#node{}, Acc) -> Acc.

sum(#node{value = Value}, Sum) -> Value + Sum.

max(Node) when is_record(Node, node) -> inorder_fold(fun max/2, null, Node).
sum(Node) when is_record(Node, node) -> inorder_fold(fun sum/2, 0, Node).

is_ordered(#node{left = Left, right = Right} = Root) ->
    is_orderedl(Root, Left) andalso is_orderedr(Root, Right).

is_orderedl(Root, null) when is_record(Root, node) -> true;
is_orderedl(#node{value = RootValue}, #node{value = LValue} = Left) -> 
    RootValue >= LValue andalso is_ordered(Left).

is_orderedr(Root, null) when is_record(Root, node) -> true;
is_orderedr(#node{value = RootValue}, #node{value = RValue} = Right) -> 
    RootValue =< RValue andalso is_ordered(Right).

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_test_() ->
    [
     ?_assertMatch(#node{value = 0}, new(0)),
     ?_assertMatch(#node{value = 1}, new(1)),
     ?_assertMatch(#node{value = 2}, new(2))
    ].

insert_test_() ->
    [
     ?_assertEqual(
        #node{right = #node{}},
        insert(0, new(0))
       ),
     ?_assertEqual(
        #node{left = #node{value = -10},
              right = #node{value = 10}},
        insert(-10, (insert(10, new(0))))
       ),
     ?_assertEqual(
        #node{left = #node{value = -10},
              right = #node{value = 10,
                            right = #node{value = 20}}},
        insert(20, insert(-10, insert(10, new(0))))
       ),
     ?_assertEqual(
        #node{left = #node{value = -10,right = #node{value = -8}}, right = #node{right = #node{value = 20, left = #node{value = 9}}}},
        insert(-8, insert(9, insert(20, insert(-10, insert(0, new(0))))))
       )
    ].

append(#node{value = Value}, List) ->
    lists:append(List, [Value]).

inorder_fold_test_() ->
    [
     ?_assertEqual(
        [0, 0],
        inorder_fold(fun append/2, [], insert(0, new(0)))
       ),
     ?_assertEqual(
        [-10, 0, 10],
        inorder_fold(fun append/2, [], insert(-10, (insert(10, new(0)))))
       ),
     ?_assertEqual(
        [-10, 0, 10, 20],
        inorder_fold(fun append/2, [], insert(20, insert(-10, insert(10, new(0)))))
       ),
     ?_assertEqual(
        [-10, -8, 0, 0, 9, 20],
        inorder_fold(fun append/2, [], insert(-8, insert(9, insert(20, insert(-10, insert(0, new(0)))))))
       )
    ].

max_test_() ->
    [
     ?_assertEqual(20, max(insert(-8, insert(9, insert(20, insert(-10, insert(0, new(0)))))))),
     ?_assertEqual(40, max(insert(40, insert(9, insert(20, insert(-10, insert(0, new(0)))))))),
     ?_assertEqual(9000, max(insert(40, insert(9, insert(20, insert(-10, insert(0, new(9000)))))))),
     ?_assertEqual(0, max(new(0)))
    ].

sum_test_() ->
    [
     ?_assertEqual(0, sum(insert(0, new(0)))),
     ?_assertEqual(0, sum(insert(-10, (insert(10, new(0)))))),
     ?_assertEqual(20, sum(insert(20, insert(-10, insert(10, new(0)))))),
     ?_assertEqual(11, sum(insert(-8, insert(9, insert(20, insert(-10, insert(0, new(0))))))))
    ].

is_ordered_test_() ->
    [
     ?_assert(is_ordered(new(0))),
     ?_assert(is_ordered(insert(0, new(0)))),
     ?_assert(is_ordered(insert(-10, (insert(10, new(0)))))),
     ?_assert(is_ordered(insert(20, insert(-10, insert(10, new(0)))))),
     ?_assert(is_ordered(insert(-8, insert(9, insert(20, insert(-10, insert(0, new(0)))))))),
     ?_assertNot(is_ordered(
                   #node{left = #node{value = 99},
                         right = #node{value = 10,
                                       right = #node{value = 20}}}))
    ].

-endif.
