-module(bf_api_web).

-export([init/1, content_types_provided/2, to_json/2, generate_etag/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

to_json(ReqData, Context) -> 
    PathInfo = wrq:path_info(ReqData),
    {ok, FuncName} = dict:find(func, PathInfo),
    Data = 
	case list_to_atom(FuncName) of
	    getActiveEventTypes -> bf_gateway:getActiveEventTypes();
	    getAllMarkets ->
		case wrq:get_qs_value("EventTypeIds", ReqData) of
		    [] -> bf_gateway:getAllMarkets();
		    QueryString -> 
			%% the input might be in form http://rs.home:8000/bf_api/getAllMarkets?EventTypeIds=7,15
			%% in this case parse ["7", "15"] to list of integers
			Tokens = string:tokens(QueryString, ","),
			EventTypeIds = [list_to_integer(Token) || Token <- Tokens],
			bf_gateway:getAllMarkets(EventTypeIds)
		end;
	    getMarket ->
		MarketId = list_to_integer(wrq:get_qs_value("MarketId", ReqData)),
		bf_gateway:getMarket(MarketId);
	    Other -> {error, Other, not_defined}
	end,
    {Data, ReqData, Context}.

generate_etag(ReqData, Context) -> {wrq:raw_path(ReqData), ReqData, Context}.
