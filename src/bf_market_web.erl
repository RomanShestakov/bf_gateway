-module(bf_market_web).

-export([init/1]).
-export([
	 allowed_methods/2,
	 process_post/2,
         resource_exists/2,
         %% last_modified/2,
         content_types_provided/2,
         %% content_types_accepted/2,
         delete_resource/2,
	 %%to_json/2,
	 to_text/2
         %% post_is_create/2,
         %% create_path/2,
         %provide_content/2
         %% accept_content/2,
         %% generate_etag/2
	]).

%-record(context, {root,response_body=undefined,metadata=[]}).

%-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.
    
allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET', 'PUT', 'DELETE', 'POST'], ReqData, Context}.

%% content_types_provided(ReqData, Context) ->
%%     {[{"application/json", to_json}], ReqData, Context}.

content_types_provided(ReqData, Context) ->
   {[{"text/plain",to_text}], ReqData, Context}.

resource_exists(ReqData, Context) ->
    case wrq:path_info(func, ReqData) of
        undefined -> {false, ReqData, Context};
        FuncName ->
	    case list_to_atom(FuncName) of
		subscribe -> {true, ReqData, subscribe};
		unsubscribe -> {true, ReqData, unsubscribe};
		show_markets -> {true, ReqData, show_markets};
		_Other -> {false, ReqData, Context}
	    end
    end.

to_text(ReqData, show_markets) ->
    Markets = bf_gateway:getSubscribedMarkets(),
    Body = io_lib:format("~p.~n", [ Markets]),
    {Body, ReqData, {}}.

%% to_json(ReqData, Result) -> 
%%     {mochijson:encode(Result), ReqData, Result}.

%% reply to post
%% hit with 
%% curl -X POST http://rs.home:8000/market/subscribe?MarketId=102940691
process_post(ReqData, subscribe) ->
    MarketId = list_to_integer(wrq:get_qs_value("MarketId", ReqData)),
    bf_gateway:subscribeToMarket(MarketId),
    {true, ReqData, {}}.

%% curl -X DELETE http://rs.home:8000/market/unsubscribe?MarketId=102873654
delete_resource(ReqData, unsubscribe) ->
    MarketId = list_to_integer(wrq:get_qs_value("MarketId", ReqData)),
    bf_gateway:unsubscribeFromMarket(MarketId),
    {true, ReqData, {}}.
