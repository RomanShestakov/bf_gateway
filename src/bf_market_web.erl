-module(bf_market_web).

-export([init/1]).
-export([
	 allowed_methods/2,
	 process_post/2,
         %% resource_exists/2,
         %% last_modified/2,
         content_types_provided/2,
         %% content_types_accepted/2,
         delete_resource/2
         %% post_is_create/2,
         %% create_path/2,
         %% provide_content/2,
         %% accept_content/2,
         %% generate_etag/2
	]).

%-record(context, {root,response_body=undefined,metadata=[]}).

%-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.
    
allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET', 'PUT', 'DELETE', 'POST'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

%% reply to post
%% hit with 
%% curl -X POST http://rs.home:8000/market/subscribe?MarketId=102940691
process_post(ReqData, Context) ->
    PathInfo = wrq:path_info(ReqData),
    {ok, FuncName} = dict:find(func, PathInfo),
    MarketId = list_to_integer(wrq:get_qs_value("MarketId", ReqData)),
    case list_to_atom(FuncName) of
	subscribe ->
	    bf_gateway:subscribeToMarket(MarketId),
	    {true, ReqData, Context};
	_Other ->
	    {false, ReqData, Context}
    end.

%% curl -X DELETE http://rs.home:8000/market/unsubscribe?MarketId=102873654
delete_resource(ReqData, Context) ->
    PathInfo = wrq:path_info(ReqData),
    {ok, FuncName} = dict:find(func, PathInfo),
    MarketId = list_to_integer(wrq:get_qs_value("MarketId", ReqData)),
    case list_to_atom(FuncName) of
	unsubscribe ->
	    bf_gateway:unsubscribeFromMarket(MarketId),
	    {true, ReqData, Context};
	_Other ->
	    {false, ReqData, Context}
    end.

