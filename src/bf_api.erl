-module(bf_api).

-export([login/3,
	logout/2]).

-include("../include/BFGlobalService.hrl").
-include("../include/BFExchangeService.hrl").
-include("../include/BFGlobalServiceErrCodes.hrl").

%%--------------------------------------------------------------------
%% @doc
%% login to betfair
%% @end
%%--------------------------------------------------------------------
-spec login(any(), string(), string()) -> {ok, string()} | {login_error, any()}.
login(GS_Wsdl, Username, Password) ->
    LoginReq = #'P:LoginReq'{
      username=Username,
      password=Password,      
      ipAddress="0",
      locationId=0,
      productId=82,
      vendorSoftwareId = 0},
    log4erl:debug("sending login req ~p", [LoginReq]),
    %% get reponse from betfair API and extract Token
    try
	LoginResp = detergent:call(GS_Wsdl, "login", [LoginReq]),
	log4erl:debug("got login resp ~p", [LoginResp]),
	case LoginResp of
	    {ok, _, [#'p:loginResponse'{ 'Result' =
					     #'P:LoginResp'{header = #'P:APIResponseHeader'{'sessionToken' = Token},
							    currency = _Currency,
							    errorCode = ErrCode,
							    minorErrorCode = MErrCode}}]} ->
		case ErrCode == ?LOGIN_ERROR_OK of
		    true -> {ok, Token};
		    false -> {login_error, {ErrCode, MErrCode}}
		end;
	    Other ->
		{login_error, Other}
	end
    catch
	Err ->
	    {login_error, Err}
    end.

%%--------------------------------------------------------------------
%% @doc
%% logout from betfair
%% @end
%%--------------------------------------------------------------------
-spec logout(any(), string()) -> {ok, string()} | {logout_error, any()}. 
logout(GS_Wsdl, Token) ->
    log4erl:debug("sending logout req"),
    LogoutReq = #'P:LogoutReq'{ 'header' = #'P:APIRequestHeader'{sessionToken = Token, clientStamp = "0" }},
    try
	LogoutResp = detergent:call(GS_Wsdl, "logout", [LogoutReq]),
	log4erl:debug("logout resp ~p", [LogoutResp]),
	case LogoutResp of
	    {ok, _, [#'p:logoutResponse'{'Result' =
					     #'P:LogoutResp'{header = #'P:APIResponseHeader'{sessionToken = NewToken},
							     errorCode = ErrCode,
							     minorErrorCode = MErrCode}}]} ->
		case ErrCode == ?LOGOUT_ERROR_OK of
		    true -> {ok, NewToken};
		    false -> {logout_error, {ErrCode, MErrCode}}
		end;
	    Other ->
		{logout_error, Other}
	end
    catch
	Err ->
	    {logout_error, Err}
    end.



