%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson
%%   modified by Grigory Starinkin, 2013

-module(mutex).

%%%===================================================================
%%% API
%%%===================================================================
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

-ifdef(TEST).
-export([cs_loop/1, locker_loop/1]).
-endif.

start() ->
    register(mutex, spawn_link(?MODULE, init, [])),
    {ok, whereis(mutex)}.

stop() ->
    mutex ! stop.

wait() ->
    mutex ! {wait, self()},
    receive ok -> ok end.

signal() ->
    mutex ! {signal, self()}, ok.

init() ->
    free().

free() ->
    receive
        {wait, Pid} ->
            Ref = erlang:monitor(process, Pid),
            Pid ! ok,
            busy({Ref, Pid});
        stop ->
            terminate()
    end.

busy({Ref, Pid}) ->
    receive
        {signal, Pid} ->
            erlang:demonitor(Ref, [flush]),
            free();
        {'DOWN', Ref, process, Pid, _Reason} ->
            free()
    end.

terminate() ->
    receive
        {wait, Pid} ->
            exit(Pid, kill),
            terminate()
    after
        0 -> ok
    end.

%%%===================================================================
%%% Tests
%%% XXX: it's to be reviewed
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


cs_start(Name) ->
    register(Name, spawn(mutex, cs_loop, [0])),
    ok.

cs_stop(Name) ->
    Name ! stop.

cs_loop(ResCount) ->
    receive
        {put, Pid} ->
            Count = ResCount - 1,
            Pid ! Count,
            cs_loop(Count);
        {take, Pid} ->
            Count = ResCount + 1,
            Pid ! Count,
            cs_loop(Count);
        stop -> ok
    end.

cs_take(Name) ->
    Name ! {take, self()},
    receive Used -> Used end.

cs_put(Name) ->
    Name ! {put, self()},
    receive Used -> Used end.

cs_test_() ->
    {setup,
     fun() ->
             cs_start(cs),
             cs
     end,
     fun(Name) ->
             cs_stop(Name)
     end,
     fun(Name) ->
             [
              ?_assertEqual(1, cs_take(Name)),
              ?_assertEqual(2, cs_take(Name)),
              ?_assertEqual(1, cs_put(Name)),
              ?_assertEqual(0, cs_put(Name))
             ]
     end
    }.

start_locker(CsName) ->
    register(locker, spawn(mutex, locker_loop, [CsName])),
    ok.

locker_loop(CsName) ->
    receive
        {lock, Pid} ->
            wait(),
            Pid ! {ok, cs_take(CsName)},
            locker_loop(CsName);
        {unlock, Pid} ->
            signal(),
            Pid ! {ok, cs_put(CsName)},
            locker_loop(CsName);
        {stop, Pid} ->
            Pid ! ok
    end.

stop_locker() ->
    locker ! {stop, self()},
    receive
        ok -> ok
    after
        500 -> {error, timeout}
    end.

lock() ->
    locker ! {lock, self()},
    receive
        {ok, Count} -> Count
    after
        500 -> {error, timeout}
    end.

unlock() ->
    locker ! {unlock, self()},
    receive
        {ok, Count} -> Count
    after
        500 -> {error, timeout}
    end.

kill_locker() ->
    case whereis(locker) of
        undefined -> {error, locker_not_spawned};
        Pid when is_pid(Pid) ->
            exit(Pid, mutex_test_exit),
            ok
    end.

locker_test() ->
    CsName = cs2,
    ?assertEqual(ok, cs_start(CsName)),
    start(),
    ?assertEqual(ok, start_locker(CsName)),
    ?assertEqual(1, lock()),
    ?assertEqual(0, unlock()),
    ?assertEqual(ok, stop_locker()),
    stop(),
    cs_stop(CsName).

mutex_start_test() ->
    {ok, Pid} = start(),
    ?assert(is_pid(Pid)),
    stop().

mutex_test_() ->
    {setup,
     fun() ->
             start(),
             CsName = mtcs,
             cs_start(CsName),
             CsName
     end,
     fun(CsName) ->
             cs_stop(CsName),
             stop()
     end,
     fun(CsName) ->
             [
              fun() ->
                      start_locker(CsName),
                      ?assertEqual(ok, wait()),
                      ?assertEqual(1, cs_take(CsName)),
                      ?assertEqual({error, timeout}, lock()),
                      ?assertEqual(0, cs_put(CsName)),
                      kill_locker(),
                      signal(),
                      timer:sleep(500),
                      ?assertEqual(ok, wait()),
                      ?assertEqual(1, cs_take(CsName)),
                      ?assertEqual(0, cs_put(CsName)),
                      signal()
              end,
              fun() ->
                      ?assertEqual(ok, start_locker(CsName)),
                      ?assertEqual(1, lock()),
                      ?assert(undefined =/= whereis(mutex)),
                      ?assertEqual(ok, kill_locker()),
                      ?assertEqual(ok, wait()),
                      ?assertEqual(2, cs_take(CsName)),
                      ?assertEqual(1, cs_put(CsName)),
                      ?assertEqual(0, cs_put(CsName)),
                      ?assert(undefined =/= whereis(mutex)),
                      signal()
              end
             ]
     end
    }.

-endif.
