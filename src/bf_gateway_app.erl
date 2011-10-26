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

-module(bf_gateway_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1, stop/0]).

-define(APPS, [inets, crypto, log4erl, bf_gateway]).
%-define(APPS, [log4erl]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% to start manually from console with start.sh
start() ->
    ssl:start(),
    io:format("~p~n", [ssl]),
    [begin application:start(A), io:format("~p~n", [A]) end || A <- ?APPS].

start(_StartType, _StartArgs) ->
    log4erl:conf(bf_util:log4erl_config()),
    log4erl:info("starting bf_gateway"),
    bf_gateway_sup:start_link().

stop() ->
    log4erl:info("stopping bf_gateway"),
    [application:stop(A) || A <- lists:reverse(?APPS)],
    ssl:stop().

stop(_State) ->
    ok.

