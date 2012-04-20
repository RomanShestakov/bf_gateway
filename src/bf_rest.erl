-module(bf_rest).

-export([init/1, to_html/2, generate_etag/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.
to_html(ReqData, Context) -> {"Hello, new world2", ReqData, Context}.
generate_etag(ReqData, Context) -> {wrq:raw_path(ReqData), ReqData, Context}.
