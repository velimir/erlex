-module(echo).
-export([start/0, print/1, stop/0]).

-compile([export_all]).

start() ->
    register(echo, spawn(echo, loop, [])),
    ok.

print(Term) ->
    case whereis(echo) of
        undefined ->
            throw({not_spawned, echo});
        Pid when is_pid(Pid) ->
            Pid ! Term,
            ok
    end.

stop() ->
    case whereis(echo) of
        undefined ->
            throw({not_spawned, echo});
        Pid when is_pid(Pid) ->
            Pid ! stop,
            ok
    end.

loop() ->
    receive
        stop -> true;
        Term ->
            io:format("~p~n", [Term]),
            loop()
    end.
