%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(fsm).

-export([idle/0,ringing/1]).
-compile([export_all]).

start() ->
    event_manager:start(fsm_logger, [{stats_handler, {start, outgoing}}]),
    init().

init() ->
    register(fsm, spawn(fsm, idle, [])).

idle() ->
    receive
        {Number, incoming} ->
            start_ringing(Number),
            ringing(Number);
        off_hook ->
            start_tone(),
            dial()
    end.

ringing(Number) ->
    receive
        {Number, other_on_hook} ->
            stop_ringing(Number),
            idle();
        {Number, off_hook} ->
            stop_ringing(Number),
            connected(Number)
    end.

connected(Number) ->
    receive
        on_hook ->
            stop_connection(Number),
            idle()
    end.

dial() ->
    receive
        on_hook ->
            stop_dial(),
            idle();
        {Number, got_number} ->
            start_dialing(Number),
            dialing(Number)
    end.

dialing(Number) ->
    receive
        other_off_hook ->
            stop_dialing(Number),
            start_outgoing(Number),
            connected(Number);
        other_on_hook ->
            stop_dialing(Number),
            idle();
        on_hook ->
            stop_dialing(Number),
            idle()
    end.

start_ringing(Number) ->
    event_manager:send_event(fsm_logger, {start, Number, ringing}).

stop_ringing(Number) ->
    event_manager:send_event(fsm_logger, {stop, Number, ringing}).

start_tone() ->
    event_manager:send_event(fsm_logger, {start, -1, tone}).

stop_tone() ->
    event_manager:send_event(fsm_logger, {stop, -1, tone}).

start_outgoing(Number) ->
    event_manager:send_event(fsm_logger, {start, Number, outgoing}).

stop_connection(Number) ->
    event_manager:send_event(fsm_logger, {stop, Number, connection}).

stop_dial() ->
    event_manager:send_event(fsm_logger, {stop, -1, dial}).

start_dialing(Number) ->
    event_manager:send_event(fsm_logger, {start, Number, dialing}).

stop_dialing(Number) ->
    event_manager:send_event(fsm_logger, {stop, Number, dialing}).
