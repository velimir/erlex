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

