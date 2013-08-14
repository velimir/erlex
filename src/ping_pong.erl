-module(ping_pong).
-export([start/0, print/1, stop/0, loop/0]).

start() ->
    register(echo, spawn_link(?MODULE, loop, [])),
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
        stop -> throw(unexpected_error);
        Term ->
            io:format("~p~n", [Term]),
            loop()
    end.
