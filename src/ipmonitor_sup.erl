%%%-------------------------------------------------------------------
%%% @author Danil Onishchenko <danil.onishchenko.info@gmail.com>
%%% @copyright (C) 2019, Danil Onishchenko
%%% @doc
%%%
%%% @end
%%% Created :  5 Jun 2019 by Danil Onishchenko <danil.onishchenko.info@gmail.com>
%%%-------------------------------------------------------------------
-module(ipmonitor_sup).

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
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SupFlags = #{
      strategy => rest_for_one,
      intensity => 1,
      period => 5
     },
    
    MonitorName = #{
      id => ipmonitor_name,
      start => {ipmonitor_name, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [ipmonitor_name]
     },
    MonitorSup = #{
      id => ipmonitor_monitor_sup,
      start => {ipmonitor_monitor_sup, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => supervisor,
      modules => [ipmonitor_monitor_sup]
     },
    
    {ok, {SupFlags, [MonitorName, MonitorSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
