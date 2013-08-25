%%%-------------------------------------------------------------------
%%% @author velimir <starinkin@gmail.com>
%%% @copyright (C) 2013, velimir
%%% @doc
%%% Exercise 12-1: Database Server Revisited
%%% @end
%%% Created : 25 Aug 2013 by velimir <starinkin@gmail.com>
%%%-------------------------------------------------------------------
-module(my_db_gen).

-behaviour(gen_server).

%% API
-export([start_link/0, start/0]).
-export([stop/0, write/2, delete/1, read/1, match/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {data}).

%%%===================================================================
%%% API
%%%===================================================================

-type(key()::any()).
-type(element()::any()).

%%--------------------------------------------------------------------
%% @doc
%% Stop the server
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
-spec(stop() -> ok).

stop() ->
    gen_server:call(?SERVER, stop).

%%--------------------------------------------------------------------
%% @doc
%% Write Key -> Element pair to the server
%%
%% @spec write(Key::any(), Element::any()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec(write(Key::key(), Element::element()) -> ok).

write(Key, Element) ->
    gen_server:call(?SERVER, {write, Key, Element}).

%%--------------------------------------------------------------------
%% @doc
%% Delete (Key.Element) pair from server by given Key
%%
%% @spec delete(Key::any()) -> ok
%% @end
%%--------------------------------------------------------------------
-spec(delete(Key::key()) -> ok).

delete(Key) ->
    gen_server:call(?SERVER, {delete, Key}).

%%--------------------------------------------------------------------
%% @doc
%% Read Element from the server by given Key
%%
%% @spec read(Key::any()) -> {ok, Element} | {error, instance}
%% @end
%%--------------------------------------------------------------------
-spec(read(Key::key()) -> {ok, element()} | {error, instance} ).

read(Key) ->
    gen_server:call(?SERVER, {read, Key}).

%%--------------------------------------------------------------------
%% @doc
%% Match all keys from in the server by given Element
%%
%% @spec match(Element::any()) -> [any()]
%% @end
%%--------------------------------------------------------------------
-spec(match(Element::element()) -> [key()]).

match(Element) ->
    gen_server:call(?SERVER, {match, Element}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server w/o linking (useful for debugging from shell)
%%
%% @spec start() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec(start() -> {ok, pid()} | ignore | {error, tuple()}).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server, create empty database
%%
%% @spec init([]) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{data=db:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({write, Key, Element}, _From, #state{data = Db} = State) ->
    NewDb = db:write(Key, Element, Db),
    {reply, ok, State#state{data = NewDb}};

handle_call({delete, Key}, _From, #state{data = Db} = State) ->
    NewDb = db:delete(Key, Db),
    {reply, ok, State#state{data = NewDb}};

handle_call({read, Key}, _From, #state{data = Db} = State) ->
    case db:read(Key, Db) of
	{error, Inst} ->
	    {reply, {error, Inst}, State};
	{ok, Val} ->
	    {reply, {ok, Val}, State}
    end;

handle_call({match, Element}, _From, #state{data = Db} = State) ->
    Match = db:match(Element, Db),
    {reply, Match, State};

handle_call(stop, _From, State) -> {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    db:destroy(State#state.data),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

comn_1_test_() ->
    {spawn,
     {inorder,
      [
       fun() ->
               {ok, Pid} =  start(),
               ?assert(is_pid(Pid))
       end,
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
