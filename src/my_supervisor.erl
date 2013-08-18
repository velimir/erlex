%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson
%%   modified by Grigory Starinkin, 2013

-module(my_supervisor).
-export([start_link/2, stop/1, start_child/5, stop_child/2]).
-export([get_id/1]).
-export([init/1]).

start_link(Name, ChildSpecList) ->
    register(Name, spawn_link(my_supervisor, init, [ChildSpecList])),
    ok.

init(ChildSpecList) ->
    process_flag(trap_exit, true),
    loop(start_children(ChildSpecList)).

start_children([]) -> [];
start_children([{M, F, A, T} | ChildSpecList]) ->
    case start_child({M, F, A, T}) of
        {Pid, Id} when is_pid(Pid) ->
            {_, Sec, _} = now(),
            [{Pid, Id, {Sec, 0}, {M,F,A,T}}|start_children(ChildSpecList)];
        {error, _Error} ->
            %% cannot apply
            start_children(ChildSpecList)
    end.

start_child({M, F, A, _T}) ->
    case (catch apply(M,F,A)) of
        {ok, Pid} ->
            io:format("~p module started - ~p~n", [M, Pid]),
            {Pid, get_id(M)};
        Error ->
            io:format("~p module not started~n", [M]),
            io:format("Error: ~p~n", [Error]),
            {error, Error}
    end.

%% XXX: tooo ugly, how to make unique id for given ModuleName
get_id(ModuleName) ->
    list_to_atom(
      binary_to_list(
        base64:encode(
          integer_to_list(lists:sum(tuple_to_list(now()))) ++ atom_to_list(ModuleName)
         ))).

start_child(Name, Module, Function, Argument, Type) ->
    Name ! {start_child, self(), {Module, Function, Argument, Type}},
    receive {reply, Reply} -> Reply end.

stop_child(Name, Id) ->
    Name ! {stop_child, self(), Id},
    receive {reply, Reply} -> Reply end. 

%% The loop of the supervisor waits in a receive clause for EXIT and stop messages.
%% If a child terminates, the supervisor receives the EXIT signal and restarts the terminated
%% child, replacing its entry in the list of children stored in the ChildList variable:

get_spawn_info({Sec, SpawnCount}) ->
    {_, NowSec, _} = now(),
    case NowSec - Sec of
        Dur when Dur < 60, SpawnCount >= 4 ->
            {error, too_many_spawns_per_min};
        Dur when Dur < 60 ->
            {Sec, SpawnCount + 1};
        Dur when Dur >= 60 ->
            {NowSec, 0}
    end.

restart_child(Pid, ChildList, Reason) ->
    {value, {Pid, Id, SpawnInfo, {M,F,A,T}}} = lists:keysearch(Pid, 1, ChildList),
    case {Reason, T} of
        {normal, transient} ->
            io:format("normal exit on transient - do not restart~n"),
            lists:keydelete(Pid,1,ChildList);
        {_, _} ->
            case get_spawn_info(SpawnInfo) of
                {error, _} ->
                    io:format("too many spawns per min: ~p, ~p deleted ~n", [Id, Pid]);
                {TimeStamp, SpawnCount} ->
                    io:format("trying to restart ~p,~p (TimeStamp: ~p, SpawnCount: ~p) ~n", [Id, Pid, TimeStamp, SpawnCount]),
                    {ok, NewPid} = apply(M,F,A),
                    io:format("~p restarted with new pid - ~p~n", [Pid, NewPid]),
                    [{NewPid, Id, {TimeStamp, SpawnCount}, {M,F,A,T}}|lists:keydelete(Pid,1,ChildList)]
            end
    end.

stop_child_impl(Id, ChildList) ->
    case lists:keysearch(Id, 2, ChildList) of
        {value, {Pid, Id, _CallTuple}} ->
            unlink(Pid),
            exit(Pid, kill),
            lists:keydelete(Id, 2, ChildList);
        false ->
            {error, no_such_id}
    end.

loop(ChildList) ->
    receive
        {'EXIT', Pid, Reason} ->
            io:format("EXIT received - ~p", [{'EXIT', Pid, Reason}]),
            NewChildList = restart_child(Pid, ChildList, Reason),
            loop(NewChildList);
        {stop, From}  ->
            From ! {reply, terminate(ChildList)};
        {start_child, From, CallTuple} ->
            case start_child(CallTuple) of
                {Pid, Id} when is_pid(Pid) ->
                    From ! {reply, {Pid, Id}},
                    loop([{Pid, Id, CallTuple}|ChildList]);
                {error, Error} ->
                    From ! {reply, {error, Error}},
                    loop(ChildList)
            end;
        {stop_child, From, Id} ->
            case stop_child_impl(Id, ChildList) of
                {error, Error} ->
                    From ! {reply, {error, Error}},
                    loop(ChildList);                
                NewChildList ->
                    From ! {reply, ok},
                    loop(NewChildList)
            end
    end.

%% We stop the supervisor by calling the synchronous client function stop/0. Upon receiving the
%% stop message, the supervisor runs through the ChildList, terminating the children one by one.
%% Having terminated all the children, the atom ok is returned to the process that initiated
%% the stop call:

stop(Name) ->
    Name ! {stop, self()},
    receive {reply, Reply} -> Reply end.

terminate([{Pid, _Id, _, _} | ChildList]) ->
    exit(Pid, kill),
    terminate(ChildList);
terminate(_ChildList) -> ok.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


sv_test() ->
    start_link(db_sv, [{my_db, start, [], permanent},
                       {mutex, start, [], permanent}]),
    %% how can we avoid using sleep?
    timer:sleep(500),
    DbPid1 = whereis(my_db),
    ?assert(is_pid(DbPid1)),
    exit(DbPid1, kill),
    %% how can we avoid using sleep?
    timer:sleep(500),
    DbPid2 = whereis(my_db),
    ?assert(is_pid(DbPid2)),
    ?assert(DbPid1 /= DbPid2),
    stop(db_sv),
    ?assert(undefined == whereis(my_db)),
    ?assert(undefined == whereis(mutex)).

transient_test() ->
    ?assertEqual(ok, start_link(db_sv, [{my_db, start, [], transient},
                                        {mutex, start, [], permanent}])),
    timer:sleep(500),
    DbPid1 = whereis(my_db),
    ?assert(is_pid(DbPid1)),
    exit(DbPid1, abnormal_exit),
    timer:sleep(500),
    DbPid2 = whereis(my_db),
    ?assert(is_pid(DbPid2)),
    ?assert(DbPid1 /= DbPid2),
    my_db:stop(),
    timer:sleep(500),
    ?assert(undefined == whereis(my_db)),
    stop(db_sv).

stop_child_test() ->
    ?assertEqual(ok, start_link(db_sv, [{my_db, start, [], transient}])),
    {_Pid, Id} = start_child(db_sv, mutex, start, [], permanent),
    ?assertEqual(ok, stop_child(db_sv, Id)),
    ?assertNot(is_pid(whereis(mutex))),
    stop(db_sv),
    ?assert(undefined == whereis(my_db)).

spawn_info_test() ->
    {_, Sec, _} = now(),
    {NextSec, NextCount} = get_spawn_info({Sec - 10, 0}),
    ?assertMatch({NextSec, 1}, {NextSec, NextCount}),
    ?assertMatch({NextSec, 2}, get_spawn_info({Sec - 10, 1})),
    ?assertMatch({NextSec, 3}, get_spawn_info({Sec - 10, 2})),
    ?assertMatch({NextSec, 4}, get_spawn_info({Sec - 10, 3})),
    ?assertMatch({error, _}, get_spawn_info({Sec - 10, 4})),
    {NewSec, NewCount} = get_spawn_info({Sec - 120, 4}),
    ?assert(NewCount == 0),
    ?assert(NewSec > NextSec).

-endif.
