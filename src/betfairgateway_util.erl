-module(betfairgateway_util).

-export([
	 log4erl_config/0,
	 get_username/0,
	 get_password/0,
	 get_GS_Wsdl/0,
	 get_GX_Wsdl/0
	]).

%%--------------------------------------------------------------------
%% @doc
%% Get log config of the project
%% creates a full path name to the job file definition.
%% @end
%%--------------------------------------------------------------------
-spec log4erl_config() -> string() | no_return().
log4erl_config() ->
    case application:get_env(betfairgateway, log4erl_config) of
	{ok, Value} -> Value;
	undefined -> throw({error, log4erl_config_not_defined})
    end.

%%--------------------------------------------------------------------
%% @doc
%% get betfair username
%% @end
%%--------------------------------------------------------------------
-spec get_username() -> string() | no_return().
get_username() ->
    case application:get_env(betfairgateway, username) of
	{ok, Value} -> Value;
	undefined -> throw({error, username_not_defined})
    end.

%%--------------------------------------------------------------------
%% @doc
%% get passord
%% @end
%%--------------------------------------------------------------------
-spec get_password() -> string() | no_return().
get_password() ->
    case application:get_env(betfairgateway, password) of
	{ok, Value} -> Value;
	undefined -> throw({error, password_not_defined})
    end.

%%--------------------------------------------------------------------
%% @doc
%% get BFGlobalService.wsdl
%% @end
%%--------------------------------------------------------------------
-spec get_GS_Wsdl() -> string().
get_GS_Wsdl() ->
    "file://" ++ code:priv_dir(betfairgateway) ++ "/BFGlobalService.wsdl".

%%--------------------------------------------------------------------
%% @doc
%% get BFExchangeService.wsdl
%% @end
%%--------------------------------------------------------------------
-spec get_GX_Wsdl() -> string().
get_GX_Wsdl() ->
    "file://" ++ code:priv_dir(betfairgateway) ++ "/BFExchangeService.wsdl".
