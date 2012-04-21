-module(bf_gateway_getActiveEventTypes).

-export([init/1, to_html/2, generate_etag/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.
to_html(ReqData, Context) -> {bf_gateway:getActiveEventTypes(), ReqData, Context}.
generate_etag(ReqData, Context) -> {wrq:raw_path(ReqData), ReqData, Context}.
