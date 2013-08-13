%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson
%%   modified by Grigory Starinkin, 2013

-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(frequency, spawn(frequency, init, [])).

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

%% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%%  The client Functions

stop()           -> call(stop).
allocate()       -> call({allocate, self()}).
deallocate(Freq) -> call({deallocate, Freq, self()}).

%% We hide all message passing and the message
%% protocol in a functional interface.

call(Message) ->
    frequency ! {request, self(), Message},
    receive
        {reply, Reply} -> Reply
    end.

%% The Main Loop

loop(Frequencies) ->
    receive
        {request, Pid, {allocate, APid}} ->
            {NewFrequencies, Reply} = allocate(Frequencies, APid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid , {deallocate, Freq, APid}} ->
            {NewFrequencies, Reply} = deallocate(Frequencies, Freq, APid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid, stop} ->
            %% stop only if all frequencies were deallocated
            case is_any_in_use(Frequencies) of
                true ->
                    reply(Pid, {error, allocated_not_empty}),
                    loop(Frequencies);
                _ ->
                    reply(Pid, ok)
            end
    end.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    case allocated_by_pid(Allocated, Pid) of
        N when N < 3 ->
            {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}};
        _ -> 
            {{[Freq|Free], Allocated}, {error, limit_per_proc_exceeded}}
    end.

allocated_by_pid(Allocated, Pid) ->
    lists:foldl(fun(It, Count) ->
                        case It of
                            {_Freq, Pid} -> Count + 1;
                            _ -> Count
                        end
                end, 0, Allocated).


deallocate({Free, Allocated}, Freq, Pid) ->
    case lists:keyfind(Freq, 1, Allocated) of
        {Freq, Pid} ->
            {{[Freq|Free], lists:keydelete(Freq, 1, Allocated)}, ok};
        {Freq, _OwnerPid} ->
            {{Free, Allocated}, {error, not_owner}};
        false ->
            {{Free, Allocated}, ok}
    end.

is_any_in_use({_Free, []}) ->
    false;
is_any_in_use({_Free, _Allocated}) ->
    true.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

freq({_, Freq}) -> Freq.

is_any_in_use_test_() ->
    [
     ?_assertEqual(false, is_any_in_use({[1, 2, 3, 4], []})),
     ?_assertEqual(true, is_any_in_use({[1, 2, 3, 4], [{10, 10}, {20, 30}]})),
     ?_assertEqual(false, is_any_in_use({[], []}))
    ].

freq_test() ->
    start(),
    %% for limit of frequencies of 3 per process
    Freq1 = allocate(),
    ?assertMatch({ok, _}, Freq1),
    Freq2 = allocate(),
    ?assertMatch({ok, _}, Freq2),
    Freq3 = allocate(),
    ?assertMatch({ok, _}, Freq3),
    ?assertEqual({error,limit_per_proc_exceeded}, allocate()),
    ?assertEqual({error, allocated_not_empty}, stop()),
    ?assertEqual(ok, deallocate(freq(Freq1))),
    ?assertEqual(ok, deallocate(freq(Freq2))),
    ?assertEqual(ok, deallocate(freq(Freq3))),
    ?assertEqual(ok, stop()).

deallocate_test_() ->
    {setup,
     fun() ->
             start(),
             allocate()
     end,
     fun({_, Freq}) ->
             deallocate(Freq),
             stop()
     end,
     %% TODO: here should be an easier way to make this fixture
     fun({_, Freq}) ->
             [
              fun() ->
                      ?assertEqual({error, not_owner}, deallocate(Freq)),
                      Freq1 = allocate(),
                      ?assertMatch({ok, _}, Freq1),
                      ?assertEqual(ok, deallocate(freq(Freq1))),
                      ?assertEqual({error, not_owner}, deallocate(Freq))
              end
             ]
     end
    }.

deallocate_2_test() ->
    {FDB, {ok, Freq}} = allocate({get_frequencies(), []}, 10),
    ?assertMatch({_, {error, not_owner}}, deallocate(FDB, Freq, 11)),
    ?assertMatch({_, ok}, deallocate(FDB, Freq, 10)).

-endif.
