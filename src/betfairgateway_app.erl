-module(betfairgateway_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1, stop/0]).

-define(APPS, [inets, log4erl, betfairgateway]).
%-define(APPS, [log4erl]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

    %% to start manually from console with start.sh
start() ->
    [begin application:start(A), io:format("~p~n", [A]) end || A <- ?APPS],
    ssl:start(),
    application:start(betfairgateway).

start(_StartType, _StartArgs) ->
    log4erl:conf(betfairgateway_util:log4erl_config()),
    log4erl:info("starting betfairgateway"),
    %%io:format("starting betfairgateway ~n"),
    betfairgateway_sup:start_link().

stop() ->
    log4erl:info("stopping betfairgateway"),
    [application:stop(A) || A <- ?APPS].

stop(_State) ->
    ok.

