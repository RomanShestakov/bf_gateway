-module(bf_api).

-export([login/2]).

-include("../include/BFGlobalService.hrl").
-include("../include/BFExchangeService.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

login(Username, Password) ->
    {Username, Password}.
