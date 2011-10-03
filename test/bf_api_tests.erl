-module(bf_api_tests).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").


-define(MARKET, {'P:Market', [{{"type","http://www.w3.org/2001/XMLSchema-instance"}, "n2:Market"}],
		 "GBR",true,1,"1317415810918","5.0", "Test desction",
		 true,"0001-01-01T00:00:00.000Z",102873781,"ACTIVE",
		 "2011-08-13T14:00:00.000Z","2011-08-13T11:45:00.000Z","O","D", "\\English Soccer\\Barclays Premier League",
		 {'P:ArrayOfEventId', [{{"type","http://www.w3.org/2001/XMLSchema-instance"},"n2:ArrayOfEventId"}],
		  [1,258597,2022802]},
		 "Top English Goalscorer 2011/12",1,2022802,
		 {'P:ArrayOfRunner', [{{"type","http://www.w3.org/2001/XMLSchema-instance"}, "n2:ArrayOfRunner"}],
		  [{'P:Runner', [{{"type","http://www.w3.org/2001/XMLSchema-instance"},"n2:Runner"}],0,"0.0","Wayne Rooney",2305696},
		   {'P:Runner', [{{"type","http://www.w3.org/2001/XMLSchema-instance"},"n2:Runner"}],0,"0.0","Darren Bent",2315604},
		   {'P:Runner', [{{"type","http://www.w3.org/2001/XMLSchema-instance"},"n2:Runner"}],0,"0.0","DJ Campbell",1339169}]},
		 [],"0.0","0.0","0.0",true,"UKT",1,
		 {'P:ArrayOfCouponLinks', [{{"nil","http://www.w3.org/2001/XMLSchema-instance"},"1"}],undefined},
		 false}).

json_encode_test_() ->
    [
      ?_assertEqual(true, is_binary(bf_json:encode(?MARKET)))
    ].

