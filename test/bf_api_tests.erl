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

-define(EXPECTED_MARKET, <<"{\"bspMarket\":false,\"countryISO3\":\"GBR\",\"couponLinks\":\"undefined\",\"discountAllowed\":true,\"eventHierarchy\":[1,258597,2022802],\"eventTypeId\":1,\"interval\":\"0.0\",\"lastRefresh\":\"1317415810918\",\"licenceId\":1,\"marketBaseRate\":\"5.0\",\"marketDescription\":\"Test desction\",\"marketDescriptionHasDate\":true,\"marketDisplayTime\":\"0001-01-01T00:00:00.000Z\",\"marketId\":102873781,\"marketStatus\":\"ACTIVE\",\"marketSuspendTime\":\"2011-08-13T14:00:00.000Z\",\"marketTime\":\"2011-08-13T11:45:00.000Z\",\"marketType\":\"O\",\"marketTypeVariant\":\"D\",\"maxUnitValue\":\"0.0\",\"minUnitValue\":\"0.0\",\"name\":\"Top English Goalscorer 2011/12\",\"numberOfWinners\":1,\"parentEventId\":2022802,\"runners\":[{\"asianLineId\":0,\"handicap\":\"0.0\",\"name\":\"DJ Campbell\",\"selectionId\":1339169},{\"asianLineId\":0,\"handicap\":\"0.0\",\"name\":\"Darren Bent\",\"selectionId\":2315604},{\"asianLineId\":0,\"handicap\":\"0.0\",\"name\":\"Wayne Rooney\",\"selectionId\":2305696}],\"runnersMayBeAdded\":true,\"timezone\":\"UKT\",\"unit\":[]}">>).

-define(MARKETDATA,<<":20158165~Match Odds~O~ACTIVE~1164223800000~\Soccer\Scottish Soccer\Bells League Div 1\Fixtures 22 November \Partick v Clyde~/1/2695886/610072/10551708/10551709/20158165~0~1~GBR~1164192924479~3~1~8737.44~N~N:">>).
-define(EXPECTED_MARKETDATA,<<"[{\"MarketId\":20158165,\"MarketName\":\"Match Odds\",\"MarketType\":\"O\",\"MarketStatus\":\"ACTIVE\",\"EventDate\":\"1164223800000\",\"MenuPath\":\"SoccerScottish SoccerBells League Div 1Fixtures 22 November Partick v Clyde\",\"EventHierarchy\":\"/1/2695886/610072/10551708/10551709/20158165\",\"BetDelay\":\"0\",\"ExchangeId\":1,\"CountryISO3\":\"GBR\",\"LastRefresh\":\"1164192924479\",\"NumberOfRunners\":3,\"NumberOfWinners\":1,\"TotalAmountMatched\":8737.44,\"BspMarket\":\"N\",\"TurningIntoPlay\":\"N\"}]">>).

-define(EVENTYPE, [{'P:EventType',[{{"type","http://www.w3.org/2001/XMLSchema-instance"},"n2:EventType"}], 189929,"Poker Room",0,0},
		   {'P:EventType',[{{"type","http://www.w3.org/2001/XMLSchema-instance"}, "n2:EventType"}], 2791893,"Yahoo Racing",0,0}]).

-define(EXPECTED_EVENTTYPE, <<"[{\"Id\":189929,\"Name\":\"Poker Room\",\"MarketId\":0,\"ExchangeId\":0},{\"Id\":2791893,\"Name\":\"Yahoo Racing\",\"MarketId\":0,\"ExchangeId\":0}]">>).


json_encode_test_() ->
    [
     ?_assertEqual(?EXPECTED_MARKET, bf_json:encode({market, ?MARKET})),
     ?_assertEqual(?EXPECTED_MARKETDATA, bf_json:encode({all_markets, ?MARKETDATA})),
     ?_assertEqual(?EXPECTED_EVENTTYPE, bf_json:encode({event_type_items, ?EVENTYPE}))
    ].

