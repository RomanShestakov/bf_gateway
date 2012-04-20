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

-module(bf_gateway_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
%% %% Helper macro for declaring children of supervisor
%% -define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Bfgw = {'bf_gateway',{'bf_gateway',start_link,[]},permanent,2000,worker,['bf_gateway']},
    Ip = 
	case bf_util:get_webmachine_ip() of
	    false -> "0.0.0.0";
	    Any -> Any
	end,
    {ok, Dispatch} = file:consult(filename:join([filename:dirname(code:which(?MODULE)),"..", "priv", "dispatch.conf"])),
    WebConfig = [{ip, Ip},
                 {port, 8000},
                 {log_dir, "log"},
                 {dispatch, Dispatch}],
    Web = {webmachine_mochiweb,{webmachine_mochiweb, start, [WebConfig]},permanent, 5000, worker, dynamic},
    {ok, { {one_for_one, 5, 10}, [Bfgw, Web]} }.

