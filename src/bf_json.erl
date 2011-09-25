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
 		   'eventHierarchy' = #'P:ArrayOfEventId'{'EventId' = EventHierarchy},
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
    iolist_to_binary(mochijson2:encode({struct, [{bspMarket, BspMarket},
						 {countryISO3, CountryISO3},
						 {couponLinks, CouponLinks},
						 {discountAllowed, DiscountAllowed},
						 {eventHierarchy, EventHierarchy},
						 {eventTypeId, EventTypeId},
						 {interval, Interval},
						 {lastRefresh, LastRefresh},
						 {licenceId, LicenceId},
						 {marketBaseRate, MarketBaseRate},
						 {marketDescription, MarketDescription},
						 {marketDescriptionHasDate, MarketDiscriptionHasDate},
						 {marketDisplayTime, MarketDisplayTime},
						 {marketId, MarketId},
						 {marketStatus, MarketStatus},
						 {marketSuspendTime, MarketSuspendTime},
						 {marketTime, MarketTime},
						 {marketType, MarketType},
						 {marketTypeVariant, MarketTypeVariant},
						 {maxUnitValue, MaxUnitValue},
						 {minUnitValue, MinUnitValue},
						 {name, Name},
						 {numberOfWinners, NumberOfWinners},
						 {parentEventId, ParentEventId},
						 {runners, Runners},
						 {runnersMayBeAdded, RunnersMayBeAdded},
						 {timezone, Timezone},
						 {unit, Unit}
						]}));
encode(Other) ->
    throw({bf_json_error, {unknown_value, Other}}).
