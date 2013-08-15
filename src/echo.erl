-module(echo).

-export([start/0, print/1, stop/0]).
-export([loop/0]).

-compile([export_all]).

start() ->
    register(echo, spawn(?MODULE, init, [])),
    ok.

init() ->
    process_flag(trap_exit, true),
    loop().

print(Term) ->
    case whereis(echo) of
        undefined ->
            throw({not_spawned, echo});
        Pid when is_pid(Pid) ->
            Pid ! Term,
            ok
    end.

spawn() ->
    echo ! {spawn, self()},
    receive P -> P end.

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
        {spawn, Pid} ->
            Spid = spawn_link(echo, test_loop, []),
            Pid ! Spid,
            loop();
        stop -> true;
        Term ->
            io:format("~p~n", [Term]),
            loop()
    end.

test_loop() ->
    receive
        stop -> ok;
        Smth ->
            io:format("~p~n", [Smth]),
            test_loop()
    end.
