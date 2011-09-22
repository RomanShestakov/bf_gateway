%%%-------------------------------------------------------------------
%%% @author Roman Shestakov <>
%%% @copyright (C) 2011, Roman Shestakov
%%% @doc
%%%
%%% @end
%%% Created : 20 Sep 2011 by Roman Shestakov <>
%%%-------------------------------------------------------------------
-module(bf_gateway).

-behaviour(gen_server).

%% -include("../include/BFExchangeService.hrl").
%% -include("../include/BFGlobalService.hrl").

%% API
-export([start_link/0]).
-export([logout/0,
	 keepalive/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(betfairgateway_util, [get_username/0,
			      get_password/0,
			      get_GS_Wsdl/0]).

-define(SERVER, ?MODULE). 

-record(state, {gs_wsdl, token}).

%%%===================================================================
%%% API
%%%===================================================================
logout() ->
    gen_server:call(?SERVER, logout).

keepalive() ->
    gen_server:call(?SERVER, keepalive).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


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
init([]) ->
    GS_Wsdl = detergent:initModel(get_GS_Wsdl()), 
    case bf_api:login(GS_Wsdl, get_username(), get_password()) of
	{ok, Token} ->
	    log4erl:info("succesfully logged to betfair"), 	    
	    {ok, #state{gs_wsdl = GS_Wsdl, token = Token}};
	{login_error, Err} ->
	    log4erl:error("error logging to betfair ~p", [Err]),
	    {stop, Err}
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

handle_call(logout, _From, #state{gs_wsdl = GS_Wsdl, token = Token} = State) ->
    case bf_api:logout(GS_Wsdl, Token) of
	{ok, NewToken} -> 
	    {reply, ok, State#state{token = NewToken}};
	Err ->
	    log4erl:error("error logging out from betfair ~p", [Err]),
	    {reply, Err, State#state{token = Token}}
    end;
handle_call(keepalive, _From, #state{gs_wsdl = GS_Wsdl, token = Token} = State) ->
    case bf_api:keepalive(GS_Wsdl, Token) of
	{ok, NewToken} -> 
	    {reply, ok, State#state{token = NewToken}};
	Err ->
	    log4erl:error("error with keepalive ~p", [Err]),
	    {reply, Err, State#state{token = Token}}
    end;
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
