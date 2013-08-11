-module(boolean).
-export([b_not/1, b_and/2, b_or/2, b_nand/2]).

b_not(true) ->
    false;
b_not(false) ->
    true;
b_not(_) ->
    {error, invalid_object}.

b_and(true, true) ->
    true;
%% I'm not sure about this case, 'cuse it pass other types of objects
b_and(_, _) ->
    false.

b_or(false, false) ->
    false;
%% So it also returns true for b_or(something, something2)
%% it's to be rewtitten in more restricted environment
b_or(_, _) ->
    true.

b_nand(Lh, Rh) ->
    b_not(b_and(Lh, Rh)).

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

b_not_test_() ->
    [
     ?_assertEqual(true, b_not(false)),
     ?_assertEqual(false, b_not(true)),
     ?_assertEqual({error, invalid_object}, b_not(not_a_boolean))
    ].

b_and_test_() ->
    [
     ?_assertEqual(true, b_and(true, true)),
     ?_assertEqual(false, b_and(false, true)),
     ?_assertEqual(false, b_and(true, false)),
     ?_assertEqual(false, b_and(false, false))
    ].

b_nand_test_() ->
    [
     ?_assertEqual(false, b_nand(true, true)),
     ?_assertEqual(true, b_nand(false, true)),
     ?_assertEqual(true, b_nand(true, false)),
     ?_assertEqual(true, b_nand(false, false))
    ].

-endif.
