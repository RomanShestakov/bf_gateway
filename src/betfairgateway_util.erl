-module(betfairgateway_util).

-compile([export_all]).

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

-spec get_username() -> string() | no_return().
get_username() ->
    case application:get_env(betfairgateway, username) of
	{ok, Value} -> Value;
	undefined -> throw({error, username_not_defined})
    end.

-spec get_password() -> string() | no_return().
get_password() ->
    case application:get_env(betfairgateway, password) of
	{ok, Value} -> Value;
	undefined -> throw({error, password_not_defined})
    end.


get_GS_Wsdl() ->
    "file://" ++ code:priv_dir(betfairgateway) ++ "/BFGlobalService.wsdl".
