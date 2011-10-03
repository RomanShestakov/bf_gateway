-module(bf_json).

-export([encode/1]).

-include("../include/BFGlobalService.hrl").
-include("../include/BFExchangeService.hrl").
-include("../include/BFGlobalServiceErrCodes.hrl").

%%--------------------------------------------------------------------
%% @doc
%% convert value "Market" returned by GetMarket call into json
%% @end
%%--------------------------------------------------------------------
%%-spec encode(#'P:Market'{}) -> binary().
encode(#'P:Market'{'countryISO3' = CountryISO3,
	 	   'discountAllowed' = DiscountAllowed,
 		   'eventTypeId' = EventTypeId,
 		   'lastRefresh' = LastRefresh,
 		   'marketBaseRate' = MarketBaseRate,
 		   'marketDescription' = MarketDescription,
 		   'marketDescriptionHasDate' = MarketDiscriptionHasDate,
 		   'marketDisplayTime' = MarketDisplayTime,
 		   'marketId' = MarketId,
 		   'marketStatus' = MarketStatus,
 		   'marketSuspendTime' = MarketSuspendTime,
 		   'marketTime' = MarketTime,
 		   'marketType' = MarketType,
 		   'marketTypeVariant' = MarketTypeVariant,
%%  		   'menuPath' = menuPath,
 		   'eventHierarchy' = EventHierarchy, %% #'P:ArrayOfEventId'{'EventId' = EventHierarchy},
 		   'name' = Name,
 		   'numberOfWinners' = NumberOfWinners,
		   'parentEventId' = ParentEventId,
 		   'runners' = #'P:ArrayOfRunner'{'Runner' = Runners},
 		   'unit' = Unit,
		   'maxUnitValue' = MaxUnitValue,
		   'minUnitValue' = MinUnitValue,
		   'interval' = Interval,
		   'runnersMayBeAdded' = RunnersMayBeAdded,
		   'timezone' = Timezone,
		   'licenceId' = LicenceId,
		   'couponLinks' = #'P:ArrayOfCouponLinks'{'CouponLink' = CouponLinks},
		   'bspMarket' = BspMarket}) ->
    try
	iolist_to_binary(
	  mochijson2:encode({struct, [ {bspMarket, BspMarket},
				       {countryISO3, list_to_binary(CountryISO3)},
				       {couponLinks, CouponLinks},
				       {discountAllowed, DiscountAllowed},
				       {eventHierarchy, eventId(EventHierarchy)},
				       {eventTypeId, EventTypeId},
				       {interval, list_to_binary(Interval)},
				       {lastRefresh, list_to_binary(LastRefresh)},
				       {licenceId, LicenceId},
				       {marketBaseRate, list_to_binary(MarketBaseRate)},
				       {marketDescription, list_to_binary(MarketDescription)},
				       {marketDescriptionHasDate, MarketDiscriptionHasDate},
				       {marketDisplayTime, list_to_binary(MarketDisplayTime)},
				       {marketId, MarketId},
				       {marketStatus, list_to_binary(MarketStatus)},
				       {marketSuspendTime, list_to_binary(MarketSuspendTime)},
				       {marketTime, list_to_binary(MarketTime)},
				       {marketType, list_to_binary(MarketType)},
				       {marketTypeVariant, list_to_binary(MarketTypeVariant)},
				       {maxUnitValue, list_to_binary(MaxUnitValue)},
				       {minUnitValue, list_to_binary(MinUnitValue)},
 				       {name, list_to_binary(Name)},
				       {numberOfWinners, NumberOfWinners},
				       {parentEventId, ParentEventId},
				       {runners, runners(Runners,[])},
				       {runnersMayBeAdded, RunnersMayBeAdded},
				       {timezone, list_to_binary(Timezone)},
				       {unit, Unit}
				     ]}))
    catch
	_:_ -> {bf_json_encode_error}
    end;

%% encode({market_data, MarketData}) ->
%%     Markets = string:tokens(MarketData, ":"),
%%     parse_market_data(Markets);

encode(Other) ->
    %%throw({bf_json_error, {unknown_value, Other}}).
    Other.

runners([], R) -> {struct, R};
runners([ #'P:Runner'{'asianLineId' = AsianLineId, 'handicap' = Handicap, 'name' = Name, 'selectionId' = SelectionId} | T], R) ->
    runners(T, [{struct,[{'asianLineId', AsianLineId},
			 {'handicap', list_to_binary(Handicap)},
			 {'name', list_to_binary(Name)},
			 {'selectionId', SelectionId}]} | R]).

eventId(#'P:ArrayOfEventId'{'EventId' = EventId}) -> EventId.
    
%% parse_market_data(Markets) ->
%%     Fields = string:tokens(Markets, "~").


%% encode(<<:MarketId
%% 	 ~MarketName
%% 	 ~MarketType
%% 	 ~MarketStatus
%% 	 ~EventDate
%% 	 ~MenuPath
%% 	 ~EventHierarchy
%% 	 ~BetDelay
%% 	 ~ExchangeId
%% 	 ~ISO3CountryCode
%% 	 ~LastRefresh
%% 	 ~NumberOfRunners
%% 	 ~NumberOfWinners
%% 	 ~TotalAmountMatched
%% 	 ~BPSMarket
%% 	 ~TurningIntoPlay,,Rest/binary>>, Acc) ->
%%     encode(Rest, []
