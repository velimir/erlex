-module(shapes).

-include("shapes.hrl").

-export([area/1, perimeter/1]).

area(#circle{radius=Radius}) ->
    math:pi() * Radius * Radius;
area(#square{side = Side}) ->
  Side * Side ;
area(#rectangle{width = Width, length = Length}) ->
    Width * Length;
area(#triangle{a = A, b = B, c = C}) ->
  S = (A + B + C)/2,
  math:sqrt(S*(S-A)*(S-B)*(S-C));
%% Other is unused 'cuse it's unused in function area clause
%% We can use _Other because it 'special' convention naming for
%% variables, that protect from generating warnings during compilation
area(_Other) ->
    {error, invalid_object}.

perimeter(#circle{radius = Radius}) ->
    2 * math:pi() * Radius;
perimeter(#rectangle{length = Length, width = Width}) ->
    Length * 2 + Width * 2;
perimeter(#triangle{a = A, b = B, c = C}) ->
    A + B + C;
perimeter(#square{side = Side}) ->
    Side *  4.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
area_square_test_() ->
    [
     ?_assertEqual(0, area(#square{side = 0})),
     ?_assertEqual(1, area(#square{side = 1})),
     ?_assertEqual(4, area(#square{side = 2}))
    ].

area_circle_test_() ->
    [
     ?_assertEqual(0.0, area(#circle{radius = 0})),
     ?_assertEqual(math:pi(), area(#circle{radius = 1})),
     %% TODO: it would be better to test with some precision
     ?_assertEqual(314.1592653589793, area(#circle{radius = 10}))
    ].

area_triangle_test_() ->
    [
     ?_assertEqual(0.0, area(#triangle{a = 0, b = 1, c = 1})),
     ?_assertEqual(0.0, area(#triangle{a = 1, b = 0, c = 1})),
     ?_assertEqual(0.0, area(#triangle{a = 1, b = 1, c = 0})),
     %% TODO: it would be better to test with some precision
     ?_assertEqual(43.30127018922193, area(#triangle{a = 10, b = 10, c = 10}))
    ].

area_rectangle_test_() ->
    [
     ?_assertEqual(0, area(#rectangle{width = 0, length = 10})),
     ?_assertEqual(0, area(#rectangle{width = 10, length = 0})),
     ?_assertEqual(100, area(#rectangle{width = 10, length = 10})),
     ?_assertEqual(1, area(#rectangle{width = 1, length = 1}))
    ].

perimeter_square_test_() ->
    [
     ?_assertEqual(0, perimeter(#square{side = 0})),
     ?_assertEqual(4, perimeter(#square{side = 1})),
     ?_assertEqual(16, perimeter(#square{side = 4}))
    ].

perimeter_circle_test_() -> 
    [
     ?_assertEqual(0.0, perimeter(#circle{radius = 0})),
     ?_assertEqual(6.283185307179586, perimeter(#circle{radius = 1}))
    ].

perimeter_triangle_test_() ->
    [
     ?_assertEqual(0, perimeter(#triangle{a = 0, b = 0, c = 0})),
     ?_assertEqual(1, perimeter(#triangle{a = 1, b = 0, c = 0})),
     ?_assertEqual(1, perimeter(#triangle{a = 0, b = 1, c = 0})),
     ?_assertEqual(1, perimeter(#triangle{a = 0, b = 0, c = 1})),
     ?_assertEqual(3, perimeter(#triangle{a = 1, b = 1, c = 1})),
     ?_assertEqual(6, perimeter(#triangle{a = 2, b = 2, c = 2}))
    ].

perimeter_rectangle_test_() ->
    [
     ?_assertEqual(20, perimeter(#rectangle{width = 0, length = 10})),
     ?_assertEqual(20, perimeter(#rectangle{width = 10, length = 0})),
     ?_assertEqual(40, perimeter(#rectangle{width = 10, length = 10})),
     ?_assertEqual(4, perimeter(#rectangle{width = 1, length = 1})),
     ?_assertEqual(0, perimeter(#rectangle{width = 0, length = 0}))
    ].

-endif.
