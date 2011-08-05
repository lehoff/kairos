%%%-------------------------------------------------------------------
%%% @author Torben Hoffmann <torben.lehoff@gmail.com>
%%% @copyright (C) 2011, Torben Hoffmann
%%% @doc
%%%
%%% @end
%%% Created : 27 Jun 2011 by Torben Hoffmann <torben.lehoff@gmail.com>
%%%-------------------------------------------------------------------
-module(chronos).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 start_link/1,
	 stop/1,
	 start_timer/4,
	 stop_timer/2
	]).



%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).


%% Types
-type server_name()   :: atom().
-type server_ref()    :: server_name() | pid().
-type timer_name()    :: term().
-type function_name() :: term().
-type args()          :: [term()].
-type callback()      :: {module(), function_name(), args()].

-record(state, 
	{running = []  :: {[timer_name(), reference()]}
	}).

%%%===================================================================
%%% API
%%%===================================================================

-start_link(server_name()) -> {'ok',pid()} | 'ignore' | {'error',term()}.    
start_link(ServerName) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [], []).

-start_link() -> {'ok',pid()} | 'ignore' | {'error',term()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec stop(server_name()) -> ok.
stop(ServerRef) ->
    gen_server:call(ServerRef, stop).
		  
-spec start_timer(server_ref(), timer_name(), pos_integer(), callback()) ->
			 'ok' | {'error',term()}.
start_timer(ServerRef, TimerName, Timeout, Callback) ->
    gen_server:call(ServerRef, {start_timer, TimerName, Timeout, Callback}).

-spec stop_timer(server_ref(), timer_name()) -> 'ok' | {'error',term()}.
stop_timer(ServerRef, TimerName) ->
    gen_server:call(ServerRef, {stop_timer, TimerName}).
		 


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


-spec init([term()]) -> {'ok', #state{}} | {'stop', term()} | 'ignore'.
init([]) ->
    {ok, #state{}}.

-spec handle_call(term(), term(), #state{}) -> 	
			 {'reply', 'ok' | {'error', term()} , #state{}} | 
			 {'stop', term(), 'ok', #state{}}.		 
handle_call({start_timer, Name, Time, Callback}, _From, 
	    #state{running=R}=State) ->
    %% If the Name timer is running we clean it up. 
    R1 = case lists:keytake(Name, 1, R) of
	     false ->
		 R;
	     {value, {_, TRef,}, Ra} ->
		 erlang:cancel_timer(TRef),
		 Ra
	 end,
    TRefNew = erlang:start_timer(Time, self(), {Name,Callback}),
    {reply, ok, State#state{running=[{Name,TRefNew}|R1]}.

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
terminate(_Reason, _State) ->
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
