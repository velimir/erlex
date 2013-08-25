%%%-------------------------------------------------------------------
%%% @author velimir <starinkin@gmail.com>
%%% @copyright (C) 2013, velimir
%%% @doc
%%% Supervisor for Database Server application
%%% @end
%%% Created : 25 Aug 2013 by velimir <starinkin@gmail.com>
%%%-------------------------------------------------------------------
-module(my_db_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 30000,
    Type = worker,

    MyDbGen = {my_db_gen, {my_db_gen, start_link, []},
	       Restart, Shutdown, Type, [my_db_gen]},

    {ok, {SupFlags, [MyDbGen]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
