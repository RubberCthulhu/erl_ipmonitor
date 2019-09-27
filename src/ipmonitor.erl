%%%-------------------------------------------------------------------
%%% @author Danil Onishchenko <danil.onishchenko.info@gmail.com>
%%% @copyright (C) 2019, Danil Onishchenko
%%% @doc
%%%
%%% @end
%%% Created :  5 Jun 2019 by Danil Onishchenko <danil.onishchenko.info@gmail.com>
%%%-------------------------------------------------------------------
-module(ipmonitor).

-compile({no_auto_import, [monitor/2, demonitor/1]}).

-behaviour(gen_server).

%% API
-export([start_link/4]).
-export([monitor/1, monitor/2, demonitor/1, status/1, check/1]).
-export([info/0, info/1]).

%% Debug
-export([debug_notify/2, debug_notify/3,
	 debug_set_status/2, debug_set_status/3,
	 debug_notify_all/2, debug_set_status_all/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% -define(SERVER, ?MODULE).
-define(SERVER(Name), {via, ipmonitor_name, Name}).
-define(IPMONITOR_TIMEOUT, 3000).

-type ip_address() :: inet:ip_address() | binary() | string().
-type ipmonitor_id() :: reference().
-type ipmonitor_info() :: map().

-record(state, {
	  name :: ipmonitor_id(),
	  addr :: inet:ip_address(),
	  orig_addr :: ip_address(),
	  status :: boolean(),
	  timer :: timer:tref(),
	  pid :: pid(),
	  monitor :: reference()
	 }).

%%%===================================================================
%%% API
%%%===================================================================
-spec monitor(Addr) -> {ok, Name, Status} | {error, Reason} when
      Addr :: ip_address(),
      Name :: ipmonitor_id(),
      Status :: boolean(),
      Reason :: term().

monitor(Addr) ->
    monitor(Addr, []).

-spec monitor(Addr, Opts) -> {ok, Name, Status} | {error, Reason} when
      Addr :: ip_address(),
      Name :: ipmonitor_id(),
      Status :: boolean(),
      Opts :: list() | map(),
      Reason :: term().

monitor(Addr, Opts) ->
    Name = ipmonitor_name:get_name(Addr),
    case ipmonitor_monitor_sup:start_child([Name, self(), Addr, Opts]) of
	{ok, _Pid} ->
	    Status = gen_server:call(?SERVER(Name), status),
	    {ok, Name, Status};
	Else ->
	    Else
    end.

-spec demonitor(Name) -> ok when
      Name :: ipmonitor_id().

demonitor(Name) ->
    gen_server:cast(?SERVER(Name), stop).

-spec status(Name) -> Status when
      Name :: ipmonitor_id(),
      Status :: boolean().

status(Name) ->
    gen_server:call(?SERVER(Name), status).

-spec check(Addr) -> Status | {error, Reason} when
      Addr :: ip_address(),
      Status :: boolean(),
      Reason :: term().

check(Addr) ->
    case parse_addr(Addr) of
	{ok, Addr1} ->
	    check_addr(Addr1);
	Error ->
	    Error
    end.

-spec info() -> [ipmonitor_info()].

info() ->
    lists:foldl(
      fun (Name, Acc) ->
	      Info = info(Name),
	      Acc#{Name => Info}
      end,
      #{}, ipmonitor_name:names()).

-spec info(ipmonitor_id()) -> ipmonitor_info().

info(Name) ->
    gen_server:call(?SERVER(Name), info, infinity).

debug_notify(Name, Status) ->
    debug_notify(Name, any, Status).

debug_notify(Name, Addr, Status) ->
    gen_server:cast(?SERVER(Name), {debug, notify, Addr, Status}).

debug_set_status(Name, Status) ->
    debug_set_status(Name, any, Status).

debug_set_status(Name, Addr, Status) ->
    gen_server:cast(?SERVER(Name), {debug, set_status, Addr, Status}).

debug_notify_all(Addr, Status) ->
    lists:foreach(
      fun (Name) ->
	      debug_notify(Name, Addr, Status)
      end,
      ipmonitor_name:names()).

debug_set_status_all(Addr, Status) ->
    lists:foreach(
      fun (Name) ->
	      debug_set_status(Name, Addr, Status)
      end,
      ipmonitor_name:names()).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name, Pid, Addr, Opts) ->
    gen_server:start_link(?SERVER(Name), ?MODULE, [Name, Pid, Addr, Opts], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Name, Pid, Addr, Opts]) ->
    case parse_addr(Addr) of
	{ok, Addr1} ->
	    case check_addr(Addr1) of
		Status when is_boolean(Status) ->
		    case prepare_opts(Opts) of
			{ok, Opts1} ->
			    Timeout = opts_get(timeout, Opts1),
			    {ok, Timer} = timer:send_interval(Timeout, check_addr),
			    Monitor = erlang:monitor(process, Pid),
			    State = #state{
				       name = Name,
				       addr = Addr1,
				       orig_addr = Addr,
				       status = Status,
				       timer = Timer,
				       pid = Pid,
				       monitor = Monitor
				      },
			    {ok, State};
			Error ->
			    {stop, Error}
		    end;
		Error ->
		    {stop, Error}
	    end;
	Error ->
	    {stop, Error}
    end.

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
handle_call(status, _From, #state{status = Status} = State) ->
    {reply, Status, State};

handle_call(info, _From, State) ->
    #state{
       name = Name,
       addr = Addr,
       orig_addr = OrigAddr,
       status = Status,
       pid = Pid
      } = State,
    Info = #{
      name => Name,
      addr => Addr,
      orig_addr => OrigAddr,
      status => Status,
      pid => Pid
     },
    {reply, Info, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({debug, notify, Address, Status}, #state{addr = Addr} = State) ->
    Yes = case Address of
	      any -> true;
	      _ ->
		  case parse_addr(Address) of
		      {ok, Addr} -> true;
		      _ -> false
		  end
	  end,
    case Yes of
	true -> notify(State#state{status = Status});
	false -> ok
    end,
    {noreply, State};

handle_cast({debug, set_status, Address, Status}, #state{addr = Addr} = State) ->
    Yes = case Address of
	      any -> true;
	      _ ->
		  case parse_addr(Address) of
		      {ok, Addr} -> true;
		      _ -> false
		  end
	  end,
    case Yes of
	true ->
	    State1 = State#state{status = Status},
	    notify(State1),
	    {noreply, State1};
	false ->
	    {noreply, State}
    end;

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
handle_info(check_addr, State) ->
    #state{
       addr = Addr,
       status = Status
      } = State,
    Status1 = case check_addr(Addr) of
		  S when is_boolean(S) ->
		      S;
		  %% Do we need to handle it?
		  _ ->
		      Status
	      end,
    case Status1 == Status of
	true ->
	    {noreply, State};
	false ->
	    State1 = State#state{status = Status1},
	    notify(State1),
	    {noreply, State1}
    end;

handle_info({'DOWN', Monitor, process, Pid, _Info}, #state{monitor = Monitor, pid = Pid} = State) ->
    {stop, normal, State};

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
terminate(_Reason, #state{monitor = Monitor, timer = Timer} = _State) ->
    erlang:demonitor(Monitor),
    timer:cancel(Timer),
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
opts_get(Key, Value) ->
    opts_get(Key, Value, undefined).

opts_get(Key, #{} = Opts, Default) ->
    maps:get(Key, Opts, Default);
opts_get(Key, Opts, Default) when is_list(Opts) ->
    proplists:get_value(Key, Opts, Default).

opts_map(Opts) when is_map(Opts) ->
    Opts;
opts_map(Opts) when is_list(Opts) ->
    opts_map1(#{}, Opts).

opts_map1(Acc, [{Key, Value} | Rest]) ->
    opts_map1(Acc#{Key => Value}, Rest);
opts_map1(Acc, [Key | Rest]) ->
    opts_map1(Acc#{Key => true}, Rest);
opts_map1(Acc, []) ->
    Acc.

opts_empty(Opts) when is_map(Opts) ->
    maps:keys(Opts) == [];
opts_empty(Opts) when is_list(Opts) ->
    Opts == [].

prepare_opts(Opts) ->
    DefaultOpts = #{
      timeout => application:get_env(ipmonitor, timeout, ?IPMONITOR_TIMEOUT)
     },
    case opts_empty(Opts) of
	true ->
	    {ok, DefaultOpts};
	false ->
	    Opts1 = maps:merge(DefaultOpts, opts_map(Opts)),
	    case check_opts(Opts1) of
		ok ->
		    {ok, Opts1};
		Error ->
		    Error
	    end
    end.

check_opts(Opts) when is_map(Opts) ->
    check_opts1(maps:to_list(Opts));
check_opts(Opts) when is_list(Opts) ->
    check_opts1(Opts).

check_opts1([{Key, Value} | Rest]) ->
    case check_opt(Key, Value) of
	true ->
	    check_opts1(Rest);
	false ->
	    {error, {badarg, Key, Value}}
    end;
check_opts1([]) ->
    ok.

check_opt(timeout, Timeout) ->
    is_integer(Timeout) andalso Timeout >= 0;
check_opt(_, _) ->
    true.

parse_addr({_, _, _, _} = Addr) ->
    {ok, Addr};
parse_addr({_, _, _, _, _, _} = Addr) ->
    {ok, Addr};
parse_addr(Addr) when is_binary(Addr) ->
    parse_addr(binary_to_list(Addr));
parse_addr(Addr) when is_list(Addr) ->
    case inet:parse_address(Addr) of
	{ok, Addr1} ->
	    {ok, Addr1};
	{error, Reason} ->
	    {error, {parse_addr, Reason}}
    end.

check_addr(Addr) ->
    case gen_udp:open(0, [inet, {ip, Addr}, {reuseaddr, true}]) of
	{ok, S} ->
	    gen_udp:close(S),
	    true;
	{error, eaddrnotavail} ->
	    false;
	Error ->
	    Error
    end.

notify(State) ->
    #state{
       name = Name,
       orig_addr = OrigAddr,
       status = Status,
       pid = Pid
      } = State,
    Pid ! {ipmonitor, Name, OrigAddr, Status}.

