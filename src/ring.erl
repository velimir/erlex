-module(ring).
%% public
-export([start/3]).
%% private fo spawn
-export([build_chain/2]).

start(M, N, Message) ->
    Pid = spawn(?MODULE, build_chain, [N - 1, self()]),
    io:format("~p(root): root created~n", [self()]),
    receive
        done ->
            io:format("~p(root): done received~n", [self()]),
            root_loop(Pid, M, Message)
    end.

root_loop(NextPid, 0, _Message) ->
    io:format("~p: root trying to stop ~p~n", [self(), NextPid]),
    NextPid ! stop,
    receive
        stop ->
            io:format("~p: root received stop~n", [self()]),
            true
    end;
root_loop(NextPid, M, Message) ->
    io:format("~p: root sending ~p to ~p~n", [self(), Message, NextPid]),
    NextPid ! Message,
    receive
        Message ->
            io:format("~p: root received ~p~n", [self(), Message]),
            root_loop(NextPid, M - 1, Message)
    end.

chain_loop(NPid) ->
    receive
        stop ->
            io:format("~p: stop received, stopping to ~p~n",
                      [self(), NPid]),
            NPid ! stop,
            true;
        Message ->
            io:format("~p: ~p received, sending to ~p~n",
                      [self(), Message, NPid]),
            NPid ! Message,
            chain_loop(NPid)
    end.

build_chain(0, ControlPid) ->
    io:format("~p: ~b process spawned~n", [self(), 0]),
    ControlPid ! done,
    chain_loop(ControlPid);
build_chain(N, ControlPid) ->
    io:format("~p: ~b process spawned~n", [self(), N]),
    chain_loop(spawn(?MODULE, build_chain, [N - 1, ControlPid])).

%% TODO: add tests
