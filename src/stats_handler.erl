-module(stats_handler).

-export([init/1, terminate/1, handle_event/2]).

init({Type, Description}) ->
    {Type, Description, 0}.

terminate(Stats) -> Stats.

handle_event({Type, _, Description}, {Type, Description, Count}) ->
    {Type, Description, Count + 1};
handle_event(_In, Stats) ->
    Stats.
