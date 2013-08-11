-module(shapes).
-export([area/1]).

area({square, Side}) ->
  Side * Side ;
area({circle, Radius}) ->
  math:pi() * Radius * Radius;
area({triangle, A, B, C}) ->
  S = (A + B + C)/2,
  math:sqrt(S*(S-A)*(S-B)*(S-C));
%% Other is unused 'cuse it's unused in function area clause
%% We can use _Other because it 'special' convention naming for
%% variables, that protect from generating warnings during compilation
area(_Other) ->
    {error, invalid_object}.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
area_square_test_() ->
    [
     ?_assertEqual(0, area({square, 0})),
     ?_assertEqual(1, area({square, 1})),
     ?_assertEqual(4, area({square, 2}))
    ].

area_circle_test_() ->
    [
     ?_assertEqual(0.0, area({circle, 0})),
     ?_assertEqual(math:pi(), area({circle, 1})),
     %% TODO: it would be better to test with some precision
     ?_assertEqual(314.1592653589793, area({circle, 10}))
    ].

area_triangle_test_() ->
    [
     ?_assertEqual(0.0, area({triangle, 0, 1, 1})),
     ?_assertEqual(0.0, area({triangle, 1, 0, 1})),
     ?_assertEqual(0.0, area({triangle, 1, 1, 0})),
     %% TODO: it would be better to test with some precision
     ?_assertEqual(43.30127018922193, area({triangle, 10, 10, 10}))
    ].

-endif.
