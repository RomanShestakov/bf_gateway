-module(bf_api_web).

-export([init/1, to_html/2, generate_etag/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.
to_html(ReqData, Context) -> 
    PathInfo = wrq:path_info(ReqData),
    {ok, FuncName} = dict:find(func, PathInfo),
    Data = 
	case list_to_atom(FuncName) of
	    getActiveEventTypes -> bf_gateway:getActiveEventTypes();
	    getAllMarkets -> bf_gateway:getAllMarkets();
	    Other -> {error, Other, not_defined}
	end,
    {Data, ReqData, Context}.

generate_etag(ReqData, Context) -> {wrq:raw_path(ReqData), ReqData, Context}.
