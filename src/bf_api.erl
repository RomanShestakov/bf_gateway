-module(bf_api).

-export([login/3]).

-include("../include/BFGlobalService.hrl").
-include("../include/BFExchangeService.hrl").
-include("../include/BFGlobalServiceErrCodes.hrl").
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

login(GS_Wsdl, Username, Password) ->
    LoginReq = #'P:LoginReq'{
      username=Username,
      password=Password,      
      ipAddress="0",
      locationId=0,
      productId=82,
      vendorSoftwareId = 0},
    
    io:format("sending login req ~p ~n", [LoginReq]),

    %%#'P:LoginResp'{'header'=Header, 'currency', 'errorCode' = ErrCode, 'minorErrorCode'= MErrCode, 'validUntil'} = 
    {ok, _, [#'p:loginResponse'{ 'Result' =  LoginRes1 }]} = detergent:call(GS_Wsdl, "login", [LoginReq]),

    #'P:LoginResp'{header = Header, errorCode = ErrCode, minorErrorCode = MErrCode} = LoginRes1,

    io:format("login resp ~p ~n", [Header]),

    %% #'P:LoginResp'{header = Header, errorCode = ErrCode, minorErrorCode = MErrCode} = 
%% 	detergent:call(GS_Wsdl, "login", [LoginReq]),
    Header.
%%     case errorCode == ?LOGIN_ERROR_OK of
%% 	true ->
%% 	    Header;
%% 	false ->
%% 	    throw({login_error, ErrCode, MErrCode})
%%     end.
