%% Copyright (C) 2011 Roman Shestakov
%%%
%%% This file is part of bf_gateway
%%%
%%% bf_gateway is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as 
%%% published by the Free Software Foundation, either version 3 of 
%%% the License, or (at your option) any later version.
%%%
%%% bf_gateway is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public 
%%% License along with Erlsom.  If not, see 
%%% <http://www.gnu.org/licenses/>.
%%%
%%% Author contact: romanshestakov@yahoo.co.uk


-module(bf_gateway).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% betfair API exposed through REST API
-export([login/2,
	 logout/0,
	 keepAlive/0,
	 getActiveEventTypes/0,
	 getAllEventTypes/0,
	 getAllMarkets/0,
	 getAllMarkets/1,
	 getMarket/1,
	 getBet/1,
	 getMarketInfo/1,
	 getMarketPricesCompressed/1,
	 getMarketPricesCompressed/2
	]).

%% manage market subscriptions
-export([subscribeToMarket/1,
	 unsubscribeFromMarket/1,
	 getSubscribedMarkets/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(bf_util, [get_username/0,
		  get_password/0,
		  get_GS_Wsdl/0,
		  get_GX_Wsdl/0
		 ]).

-define(SERVER, ?MODULE). 
-define(KEEP_ALIVE_TIMEOUT, 900000). %% 15min

-record(state, {gs_wsdl, gx_wsdl, token, publishedMarketPids = [], publisher}).

%%%===================================================================
%%% API
%%%===================================================================

login(Username, Password) ->
    gen_server:call({global, ?SERVER}, {login, Username, Password}).

logout() ->
    gen_server:call({global, ?SERVER}, logout).

keepAlive() ->
    gen_server:call({global, ?SERVER}, keepalive).

 
%%--------------------------------------------------------------------
%% @doc
%% allows the customer to retrieve lists of all categories of sporting events (Games, Event Types)
%% that are available to bet on: in other words, all those that have at least one currently active
%% or suspended market associated with them. This means, therefore, that the service would, for example,
%% always return the event types Soccer and Horse Racing but would not return Olympics 2004 or EURO 2004 after those events had finished.
%% @end
%%--------------------------------------------------------------------
%-spec
getActiveEventTypes() ->
    gen_server:call({global, ?SERVER}, getActiveEventTypes).

%%--------------------------------------------------------------------
%% @doc
%% service allows the customer to retrieve lists of all categories of sports (Games, Event Types) that have at least one market associated with them, regardless of whether that market is now closed for betting. This means that, for example, the service would always return the event types Soccer and Horse Racing and would also return Olympics 2004 or EURO 2004 for a certain period after the markets for those events had closed; it would also return Olympics 2004 or EURO 2004 for a certain period before the markets for those events had opened. The service returns information on future events to allow API programmers to see the range of events that will beavailable to bet on in the near future.
%%
%% @end
%%--------------------------------------------------------------------
getAllEventTypes() ->
    gen_server:call({global, ?SERVER}, getAllEventTypes).

 
%%--------------------------------------------------------------------
%% @doc
%% service allows you to retrieve information about all of the markets that are currently active or suspended on the given exchange.You can use this service to quickly analyse the available markets on the exchange, or use the response to build a local copy of the Betfair.com navigation menu. You can limit the response to a particular time period, country where the event is taking place, and event type. Otherwise, the service returns all active and suspended markets.
%% @end
%%--------------------------------------------------------------------
getAllMarkets() ->
    getAllMarkets(nil).

getAllMarkets(EventTypeIds) ->
    gen_server:call({global, ?SERVER}, {getAllMarkets, EventTypeIds}).

getMarket(MarketId) ->
    gen_server:call({global, ?SERVER}, {getMarket, MarketId}).

getBet(BetId) ->
    gen_server:call({global, ?SERVER}, {getBet, BetId}).

getMarketInfo(MarketId) ->
    gen_server:call({global, ?SERVER}, {getMarketInfo, MarketId}).

getMarketPricesCompressed(MarketId) ->
    gen_server:call({global, ?SERVER}, {getMarketPricesCompressed, MarketId, default}).

getMarketPricesCompressed(MarketId, Format) ->
    gen_server:call({global, ?SERVER}, {getMarketPricesCompressed, MarketId, Format}).

subscribeToMarket(MarketId) ->
    bf_publisher:subscribeToMarket(MarketId).

unsubscribeFromMarket(MarketId) ->
    bf_publisher:unsubscribeFromMarket(MarketId).

getSubscribedMarkets() ->
    bf_publisher:getSubscribedMarkets().
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

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
    process_flag(trap_exit, true),
    %% init SOAP service wsdl files
    try
	GS_Wsdl = detergent:initModel(get_GS_Wsdl()),
	GX_Wsdl = detergent:initModel(get_GX_Wsdl()),
	%% login to betfair
	log4erl:info("logging to betfair..."),
	%% at this point the message loop is not started yet, so we call bf_api:login directly
	case bf_api:login(GS_Wsdl, get_username(), get_password()) of
	    {ok, Token} ->
		log4erl:info("succesfully logged to betfair"),
		%% start keepalive loop to make sure the connection won't timeout
		spawn_link(fun() -> keepalive_timer(?KEEP_ALIVE_TIMEOUT) end),
		%% start publisher, which takes care of communication with 0mq.
		Pid = bf_publisher:start_link(), 
		{ok, #state{gs_wsdl = GS_Wsdl, gx_wsdl = GX_Wsdl, token = Token, publisher = Pid}};
	    {login_error, Err} ->
		log4erl:error("error logging to betfair ~p", [Err]),
		{stop, Err}
	end
    catch
	_:X -> log4erl:error(X)
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

handle_call({login, Username, Password}, _From, #state{gs_wsdl = GS_Wsdl, token = Token} = State) ->
    case bf_api:login(GS_Wsdl, Username, Password) of
	{ok, NewToken} ->
	    log4erl:info("succesfully logged to betfair"), 
	    {reply, ok, State#state{token = NewToken}};
	Err ->
	    log4erl:error("error logging to betfair ~p", [Err]),
	    {reply, Err, State#state{token = Token}}
    end;
handle_call(logout, _From, #state{gs_wsdl = GS_Wsdl, token = Token} = State) ->
    case bf_api:logout(GS_Wsdl, Token) of
	{ok, NewToken} -> 
	    log4erl:info("succesfully logged out from betfair"),
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
handle_call(getActiveEventTypes, _From, #state{gs_wsdl = GS_Wsdl, token = Token} = State) ->
    case bf_api:getActiveEventTypes(GS_Wsdl, Token) of
	{ok, NewToken, EventTypes} -> 
	    {reply, EventTypes, State#state{token = NewToken}};
	{getActiveEventTypes_error, {ErrCode, _MErrCode}, NToken} ->
	    log4erl:error("error with getActiveEventTypes ~p", [ErrCode]),
	    {reply, ErrCode, State#state{token = NToken}};
	{Other, Reason} ->
	    log4erl:error("~p error in getActiveEventTypes call ~p", [Other, Reason]),
	    {reply, Reason, State#state{token = Token}}
    end;
handle_call(getAllEventTypes, _From, #state{gs_wsdl = GS_Wsdl, token = Token} = State) ->
    case bf_api:getAllEventTypes(GS_Wsdl, Token) of
	{ok, NewToken, EventTypes} -> 
	    {reply, EventTypes, State#state{token = NewToken}};
	{getAllEventTypes_error, {ErrCode, _MErrCode}, NToken} ->
	    log4erl:error("error with getAllEventTypes ~p", [ErrCode]),
	    {reply, ErrCode, State#state{token = NToken}};
	{Other, Reason} ->
	    log4erl:error("~p error in getAllEventTypes call ~p", [Other, Reason]),
	    {reply, Reason, State#state{token = Token}}
    end;
handle_call({getAllMarkets, EventTypeIds}, _From, #state{gx_wsdl = GX_Wsdl, token = Token} = State) ->
    case bf_api:getAllMarkets(GX_Wsdl, Token, EventTypeIds) of
	{ok, NewToken, MarketData} ->
	    {reply, MarketData, State#state{token = NewToken}};
	{getAllMarkets_error, {ErrCode, _MErrCode}, NToken} ->
	    log4erl:error("error with getAllMarkets ~p", [ErrCode]),
	    {reply, ErrCode, State#state{token = NToken}};
	{Other, Reason} ->
	    log4erl:error("~p error in getAllMarket call ~p", [Other, Reason]),
	    {reply, Reason, State#state{token = Token}}
    end;
handle_call({getMarket, MarketId}, _From, #state{gx_wsdl = GX_Wsdl, token = Token} = State) ->
    case bf_api:getMarket(GX_Wsdl, Token, MarketId) of
	{ok, NewToken, Market} ->
	    {reply, Market, State#state{token = NewToken}};
	{getMarket_error, Err, NToken} ->
	    log4erl:error("error in getMarket call ~p", [Err]),
 	    {reply, Err, State#state{token = NToken}};
	{Other, Reason} ->
	    log4erl:error("~p error in getMarket call ~p", [Other, Reason]),
 	    {reply, Reason, State#state{token = Token}}
    end;
handle_call({getMarketInfo, MarketId}, _From, #state{gx_wsdl = GX_Wsdl, token = Token} = State) ->
    case bf_api:getMarketInfo(GX_Wsdl, Token, MarketId) of
	{ok, NewToken, Market} ->
	    {reply, Market, State#state{token = NewToken}};
	{getMarketInfo_error, Err, NToken} ->
	    log4erl:error("error in getMarketInfo call ~p", [Err]),
 	    {reply, Err, State#state{token = NToken}};
	{Other, Reason} ->
	    log4erl:error("~p error in getMarketInfo call ~p", [Other, Reason]),
 	    {reply, Reason, State#state{token = Token}}
    end;
handle_call({getBet, BetId}, _From, #state{gx_wsdl = GX_Wsdl, token = Token} = State) ->
    case bf_api:getBet(GX_Wsdl, Token, BetId) of
	{ok, NewToken, Bet} ->
	    {reply, Bet, State#state{token = NewToken}};
	{getBet_error, Err, NToken} ->
	    log4erl:error("error in getBet call ~p", [Err]),
 	    {reply, Err, State#state{token = NToken}};
	{Other, Reason} ->
	    log4erl:error("~p error in getBet call ~p", [Other, Reason]),
 	    {reply, Reason, State#state{token = Token}}
    end;
handle_call({getMarketPricesCompressed, MarketId, Format}, _From, #state{gx_wsdl = GX_Wsdl, token = Token} = State) ->
    case bf_api:getMarketPricesCompressed(GX_Wsdl, Token, MarketId, Format) of
	{ok, NewToken, MarketPrices} ->
	    {reply, MarketPrices, State#state{token = NewToken}};
	{getMarketPricesCompressed_error, Err, NToken} ->
	    log4erl:error("error in getMarketPricesCompressed call ~p", [Err]),
 	    {reply, Err, State#state{token = NToken}};
	{Other, Reason} ->
	    log4erl:error("~p error in getMarketPricesCompressed call ~p", [Other, Reason]),
 	    {reply, Reason, State#state{token = Token}}
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
    logout(),
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
%%--------------------------------------------------------------------
%% @doc
%% recursively call keepalive to keep connection to betfair open
%% @end
%%--------------------------------------------------------------------
-spec keepalive_timer(integer()) -> no_return().
keepalive_timer(Timeout) ->
    timer:sleep(Timeout),
    log4erl:info("sending keepalive..."),
    bf_gateway:keepAlive(),
    keepalive_timer(Timeout).
