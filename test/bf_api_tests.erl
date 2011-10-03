-module(bf_api_tests).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% -define(EXPECTED, 
%% <<"digraph mdigraph{
%% ratio=compress;
%% ranksep=.75;
%% size=\"7.5, 7.5\";
%% \"C\"->\"C~1\";
%% \"C\"->\"C~0\";
%% \"A\"->\"B\";
%% \"B\"->\"C\";
%% \"C\"[style=filled,fillcolor=green,shape=hexagon,]
%% \"A\"[style=filled,fillcolor=green,shape=ellipse,]
%% \"B\"[style=filled,fillcolor=green,shape=box,]
%% \"C~1\"[style=filled,fillcolor=lightblue,shape=box,]
%% \"C~0\"[style=filled,fillcolor=lightblue,shape=box,]
%% }">>).



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

-define(EXPECTED, <<"{\"bspMarket\":false,\"countryISO3\":\"GBR\",\"couponLinks\":\"undefined\",\"discountAllowed\":true,\"eventHierarchy\":[1,258597,2022802],\"eventTypeId\":1,\"interval\":\"0.0\",\"lastRefresh\":\"1317415810918\",\"licenceId\":1,\"marketBaseRate\":\"5.0\",\"marketDescription\":\"Test desction\",\"marketDescriptionHasDate\":true,\"marketDisplayTime\":\"0001-01-01T00:00:00.000Z\",\"marketId\":102873781,\"marketStatus\":\"ACTIVE\",\"marketSuspendTime\":\"2011-08-13T14:00:00.000Z\",\"marketTime\":\"2011-08-13T11:45:00.000Z\",\"marketType\":\"O\",\"marketTypeVariant\":\"D\",\"maxUnitValue\":\"0.0\",\"minUnitValue\":\"0.0\",\"name\":\"Top English Goalscorer 2011/12\",\"numberOfWinners\":1,\"parentEventId\":2022802,\"runners\":{\"struct\":{\"asianLineId\":0,\"handicap\":\"0.0\",\"name\":\"DJ Campbell\",\"selectionId\":1339169},\"struct\":{\"asianLineId\":0,\"handicap\":\"0.0\",\"name\":\"Darren Bent\",\"selectionId\":2315604},\"struct\":{\"asianLineId\":0,\"handicap\":\"0.0\",\"name\":\"Wayne Rooney\",\"selectionId\":2305696}},\"runnersMayBeAdded\":true,\"timezone\":\"UKT\",\"unit\":[]}">>).


json_encode_test_() ->
    [
     ?_assertEqual(?EXPECTED, bf_json:encode(?MARKET))
    ].

