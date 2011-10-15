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


-define(BET, {'P:Bet',
	      [{{"type",
		 "http://www.w3.org/2001/XMLSchema-instance"},
		"n2:Bet"}],
	      0,"4.6","16621286071","S","B","E","NONE",
	      "0001-01-01T00:00:00.000Z",
	      "0001-01-01T00:00:00.000Z",103971753,
	      "R4 2800m Hcap",
	      "RSA / Arl (RSA) 7th Oct / 13:05 R4 2800m Hcap","O",
	      "D","2011-10-07T11:52:23.000Z","5.0",
	      {'P:ArrayOfMatch',
	       [{{"type",
		  "http://www.w3.org/2001/XMLSchema-instance"},
		 "n2:ArrayOfMatch"}],
	       [{'P:Match',
		 [{{"type",
		    "http://www.w3.org/2001/XMLSchema-instance"},
		   "n2:Match"}],
		 "S","2011-10-07T11:52:23.000Z","4.6","6.5",
		 "2011-10-07T12:19:46.000Z","5.0","22859105905",
		 "0001-01-01T00:00:00.000Z"}]},
	      "2011-10-07T11:52:23.000Z","4.1","0.0","6.5",
	      5508037,"October Club","2011-10-07T12:19:46.000Z",
	      "0.0","5.0","0001-01-01T00:00:00.000Z","0.0"}).
	
-define(EXPECTED_BET, <<"{\"asianLineId\":0,\"avgPrice\":4.6,\"betId\":16621286071,\"betStatus\":\"S\",\"betType\":\"B\",\"betCategoryType\":\"E\",\"betPersistenceType\":\"NONE\",\"cancelledDate\":\"0001-01-01T00:00:00.000Z\",\"lapsedDate\":\"0001-01-01T00:00:00.000Z\",\"marketId\":103971753,\"marketName\":\"R4 2800m Hcap\",\"fullMarketName\":\"RSA / Arl (RSA) 7th Oct / 13:05 R4 2800m Hcap\",\"marketType\":\"O\",\"marketTypeVariant\":\"D\",\"matchedDate\":\"2011-10-07T11:52:23.000Z\",\"matchedSize\":5.0,\"matches\":[{\"betStatus\":\"S\",\"matchedDate\":\"2011-10-07T11:52:23.000Z\",\"priceMatched\":4.6,\"profitLoss\":6.5,\"settledDate\":\"2011-10-07T12:19:46.000Z\",\"sizeMatched\":5.0,\"transactionId\":22859105905,\"voidedDate\":\"0001-01-01T00:00:00.000Z\"}],\"placedDate\":\"2011-10-07T11:52:23.000Z\",\"price\":4.1,\"bspLiability\":0.0,\"profitAndLoss\":6.5,\"selectionId\":5508037,\"selectionName\":\"October Club\",\"settledDate\":\"2011-10-07T12:19:46.000Z\",\"remainingSize\":0.0,\"requestedSize\":5.0,\"handicap\":0.0}">>).

-define(MARKETLITE,{'P:MarketLite', [{{"type", "http://www.w3.org/2001/XMLSchema-instance"},"n2:MarketLite"}],
		    "ACTIVE","2011-08-13T14:00:00.000Z",
		    "2011-08-13T11:45:00.000Z",27,0,false,false}).

-define(EXPECTED_MARKETLITE, <<"{\"marketStatus\":\"ACTIVE\",\"marketSuspendTime\":\"2011-08-13T14:00:00.000Z\",\"marketTime\":\"2011-08-13T11:45:00.000Z\",\"numberofRunners\":27,\"delay\":0,\"reconciled\":false,\"openForBspBetting\":false}">>).


-define(MARKET_PRICE, <<"102873781~GBP~ACTIVE~0~1~~true~5.0~1318367242594~6. Earlswood,9.08,2.5;8. FanMail,9.07,2.4;~N:2305696~0~12052.48~1.31~~~false~~~~|1.31~14.0~L~1~1.3~20.03~L~2~1.28~63.9~L~3~|1.33~161.79~B~1~1.46~50.0~B~2~1.48~8.0~B~3~:2315604~1~2985.94~23.0~~~false~~~~|13.0~2.0~L~1~10.5~2.0~L~2~10.0~5.0~L~3~|50.0~2.0~B~1~75.0~4.0~B~2~:2307211~2~750.46~13.5~~~false~~~~|14.5~2.0~L~1~13.0~2.0~L~2~3.5~2.0~L~3~|25.0~4.0~B~1~26.0~2.0~B~2~36.0~14.41~B~3~:2531613~3~5478.32~34.0~~~false~~~~|34.0~6.71~L~1~14.0~4.0~L~2~13.0~2.0~L~3~|:2307188~4~1161.0~11.0~~~false~~~~|23.0~9.7~L~1~22.0~6.0~L~2~11.0~146.92~L~3~|:2305714~5~436.74~150.0~~~false~~~~|26.0~5.0~L~1~25.0~9.66~L~2~13.5~2.17~L~3~|65.0~8.0~B~1~:2324718~6~238.14~17.5~~~false~~~~|40.0~12.88~L~1~18.5~2.0~L~2~16.5~3.5~L~3~|75.0~2.0~B~1~:2324721~7~200.6~27.0~~~false~~~~|17.5~5.0~L~1~17.0~6.44~L~2~11.0~153.24~L~3~|:131151~8~187.52~100.0~~~false~~~~|65.0~2.56~L~1~55.0~10.0~L~2~16.0~8.0~L~3~|:2315617~9~452.98~42.0~~~false~~~~|50.0~2.0~L~1~48.0~6.44~L~2~13.0~7.0~L~3~|:2303276~10~273.36~38.0~~~false~~~~|24.0~2.0~L~1~23.0~6.44~L~2~20.0~5.0~L~3~|:2582095~11~219.42~85.0~~~false~~~~|60.0~3.0~L~1~55.0~6.44~L~2~25.0~5.0~L~3~|:2340624~12~46.0~75.0~~~false~~~~|40.0~6.5~L~1~38.0~2.0~L~2~11.5~39.0~L~3~|:2311046~13~159.76~250.0~~~false~~~~|60.0~3.2~L~1~14.0~5.0~L~2~13.5~2.0~L~3~|250.0~2.0~B~1~1000.0~2.01~B~2~:2303327~14~8.0~300.0~~~false~~~~|100.0~6.41~L~1~19.0~21.0~L~2~18.5~32.0~L~3~|:41351~15~69.54~200.0~~~false~~~~|100.0~6.47~L~1~50.0~2.0~L~2~11.5~38.0~L~3~|:1378318~16~142.72~50.0~~~false~~~~|60.0~6.39~L~1~13.5~2.0~L~2~13.0~20.8~L~3~|:2312500~17~21.06~300.0~~~false~~~~|190.0~3.0~L~1~180.0~6.44~L~2~13.0~2.0~L~3~|:3402026~18~8.0~510.0~~~false~~~~|310.0~4.56~L~1~300.0~2.0~L~2~3.5~2.0~L~3~|:2878351~19~80.92~100.0~~~false~~~~|40.0~6.44~L~1~17.5~12.0~L~2~17.0~96.1~L~3~|:41353~20~8.9~300.0~~~false~~~~|100.0~6.41~L~1~17.5~34.0~L~2~17.0~96.07~L~3~|:2312496~21~70.14~120.0~~~false~~~~|21.0~10.0~L~1~20.0~12.79~L~2~13.0~2.0~L~3~|:2333484~22~61.34~120.0~~~false~~~~|100.0~2.0~L~1~3.55~4.0~L~2~3.5~2.0~L~3~|:2831797~23~201.28~23.0~~~false~~~~|26.0~2.0~L~1~25.0~11.5~L~2~11.0~10.0~L~3~|80.0~2.0~B~1~:4103467~24~48.52~70.0~~~false~~~~|110.0~7.2~L~1~100.0~2.0~L~2~3.5~2.0~L~3~|:2385828~25~66.54~200.0~~~false~~~~|200.0~3.22~L~1~100.0~2.0~L~2~3.5~2.0~L~3~|1000.0~2.0~B~1~:1339169~26~81.16~90.0~~~false~~~~|60.0~2.56~L~1~14.0~5.0~L~2~13.5~2.0~L~3~|500.0~2.0~B~1~">>).



json_encode_test_() ->
    [
     ?_assertEqual(?EXPECTED_MARKET, bf_json:encode({market, ?MARKET})),
     ?_assertEqual(?EXPECTED_MARKETDATA, bf_json:encode({all_markets, ?MARKETDATA})),
     ?_assertEqual(?EXPECTED_EVENTTYPE, bf_json:encode({event_type_items, ?EVENTYPE})),
     ?_assertEqual(?EXPECTED_BET, bf_json:encode({bet, ?BET})),
     ?_assertEqual(?EXPECTED_MARKETLITE, bf_json:encode({marketInfo, ?MARKETLITE}))
    ].

