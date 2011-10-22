%% Copyright (C) 2011 Roman Shestakov
%%%
%%% This file is part of betfairgateway
%%%
%%% betfairgateway is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as 
%%% published by the Free Software Foundation, either version 3 of 
%%% the License, or (at your option) any later version.
%%%
%%% betfairgateway is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public 
%%% License along with Erlsom.  If not, see 
%%% <http://www.gnu.org/licenses/>.
%%%
%%% Author contact: romanshestakov@yahoo.co.uk

-module(bf_publisher).

-behaviour(gen_server).

-define(SERVER, ?MODULE). 
-define(BETFAIR_API_HIT_TIMEOUT, 10000). %% 10sec for now to avoid throttling

%% API
-export([start_link/0, publish_price/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {publishedMarketPids = [], context, publisher}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

publish_price(Msg) ->
    gen_server:cast(?SERVER, {publish, Msg}).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initializes the server
%%--------------------------------------------------------------------
init([]) ->
    log4erl:info("starting publisher"),
    %% trap exit so clean up would work
    process_flag(trap_exit, true),
    %% Prepare our context and publisher
    {ok, Context} = erlzmq:context(),
    {ok, Publisher} = erlzmq:socket(Context, pub),
    ok = erlzmq:bind(Publisher, "tcp://*:5556"),
    {ok, #state{context = Context, publisher = Publisher}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({publish, Msg}, State) ->
    %%log4erl:info("PUBLISHING ~p", [Msg]),
    ok = erlzmq:send(State#state.publisher, Msg),
    {noreply, State};
handle_cast({subscribeToMarket, MarketId}, State) ->
    case proplists:is_defined(MarketId, State#state.publishedMarketPids) of
	false ->
	    %% market is not being published, start publishing process.
	    log4erl:info("start publishing prices for Market ~p", [MarketId]),
	    Pid = spawn_link(fun() -> run_publisher(MarketId) end),
	    {noreply, State#state{publishedMarketPids = [{MarketId, Pid} | State#state.publishedMarketPids]}};
	true ->
	    {noreply, State}
    end;
handle_cast({unsubscribeFromMarket, MarketId}, State) ->
    case proplists:lookup(MarketId, State#state.publishedMarketPids) of
	{MarketId, Pid} ->
	    log4erl:info("stop publishing prices for Market ~p", [MarketId]),
	    Pid ! cancel,
	    {noreply, State#state{publishedMarketPids = proplists:delete(MarketId, State#state.publishedMarketPids)}};
	none ->
	    log4erl:error("not subscribed to market ~p", [MarketId]),
	    {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    erlzmq:close(State#state.publisher),
    erlzmq:term(State#state.context),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% recursiverly call getMarketPricesCompressed for a given market and publish quotes to ZeroMQ
%% @end
%%--------------------------------------------------------------------
-spec run_publisher(integer()) -> no_return().
run_publisher(MarketId) ->
    receive
	cancel -> void
    after ?BETFAIR_API_HIT_TIMEOUT ->
	    Quote = bf_gateway:getMarketPricesCompressed(MarketId),
	    bf_publisher:publish_price(Quote),
	    run_publisher(MarketId)
    end.
