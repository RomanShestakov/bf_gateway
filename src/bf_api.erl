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
    
    log4erl:debug("sending login req ~p", [LoginReq]),
    
    %% get reponse from betfair API and extract Token
    {ok, _, [#'p:loginResponse'{ 'Result' =
				     #'P:LoginResp'{header = #'P:APIResponseHeader'{'sessionToken' = Token},
						    currency = _Currency,
						    errorCode = ErrCode,
						    minorErrorCode = MErrCode}}]}
	= LoginResp = detergent:call(GS_Wsdl, "login", [LoginReq]),

    log4erl:debug("got login resp ~p", [LoginResp]),

    case ErrCode == ?LOGIN_ERROR_OK of
	true -> Token;
	false -> throw({login_error, ErrCode, MErrCode})
    end.
