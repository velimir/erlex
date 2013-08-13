-module(my_db).

%%%===================================================================
%%% API
%%%
%%% my_db:start() ⇒ ok.
%%% my_db:stop() ⇒ ok.
%%% my_db:write(Key, Element) ⇒ ok.
%%% my_db:delete(Key) ⇒ ok.
%%% my_db:read(Key) ⇒ {ok, Element} | {error, instance}.
%%% my_db:match(Element) ⇒ [Key1, ..., KeyN].
%%%
%%%===================================================================

-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([init/0]).

start() ->
    register(my_db, spawn(my_db, init, [])),
    ok.

stop() -> call(stop).
write(Key, Element) -> call({write, Key, Element}).
delete(Key) -> call({delete, Key}).
read(Key) -> call({read, Key}).
match(Element) -> call({match, Element}).

call(Request) ->
    my_db ! {request, self(), Request},
    receive {reply, Reply} -> Reply end.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.

init() -> loop(db:new()).

loop(DbState) ->
    receive
        {request, Pid, {write, Key, Element}} ->
            NewDb = db:write(Key, Element, DbState),
            reply(Pid, ok),
            loop(NewDb);
        {request, Pid, {delete, Key}} ->
            NewDb = db:delete(Key, DbState),
            reply(Pid, ok),
            loop(NewDb);
        {request, Pid, {read, Key}} ->
            case db:read(Key, DbState) of
                {error, Inst} ->
                    reply(Pid, {error, Inst});
                {ok, Val} ->
                    reply(Pid, {ok, Val})
            end,
            loop(DbState);
        {request, Pid, {match, Element}} ->
            Match = db:match(Element, DbState),
            reply(Pid, Match),
            loop(DbState);
        {request, Pid, stop} ->
            reply(Pid, ok)
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

comn_1_test_() ->
    {spawn,
     {inorder,
      [
       ?_assertEqual(ok, start()),
       ?_assertEqual(ok, write(francesco, london)),
       ?_assertEqual(ok, write(lelle, stockholm)),
       ?_assertEqual({ok,london}, read(francesco)),
       ?_assertEqual(ok, write(joern, stockholm)),
       ?_assertEqual({error,instance}, read(ola)),
       ?_assertEqual([joern,lelle], match(stockholm)),
       ?_assertEqual(ok, delete(lelle)),
       ?_assertEqual([joern], match(stockholm)),
       ?_assertEqual(ok, stop())
      ]
     }
    }.

-endif.
