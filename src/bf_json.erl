%% Copyright (C) 2011 Roman Shestakov
%%%
%%% This file is part of betfairgateway
%%%
%%% betfairgateway is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as 
%%% published by the Free Software Foundation, either version 3 of 
%%% the License, or (at your option) any later version.
%%%
%%% betfairgateway is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public 
%%% License along with Erlsom.  If not, see 
%%% <http://www.gnu.org/licenses/>.
%%%
%%% Author contact: romanshestakov@yahoo.co.uk

-module(bf_json).

-compile(export_all).

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
encode({market, #'P:Market'{'countryISO3' = CountryISO3,
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
			    'bspMarket' = BspMarket}}) ->
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

encode({marketInfo, #'P:MarketLite'{'marketStatus' = MarketStatus,
				    'marketSuspendTime' = MarketSuspendTime,
				    'marketTime' = MarketTime,
				    'numberOfRunners' = NumberOfRunners,
				    'delay' = Delay,
				    'reconciled' = Reconciled,
				    'openForBspBetting'	= OpenBspBetting}}) ->
    try
	iolist_to_binary(
	  mochijson2:encode({struct, [{'marketStatus', list_to_binary(MarketStatus)},
				      {'marketSuspendTime', list_to_binary(MarketSuspendTime)},
				      {'marketTime', list_to_binary(MarketTime)},
				      {'numberofRunners', NumberOfRunners},
				      {'delay', Delay},
 				      {'reconciled', Reconciled},
 				      {'openForBspBetting', OpenBspBetting}
				     ]}))
    catch
	_:_ -> {bf_json_encode_error}
    end;

encode({event_type_items, EventTypeItems}) ->
    iolist_to_binary(
      mochijson2:encode([{struct, [{'Id', Id},
				   {'Name', list_to_binary(Name)},
				   {'MarketId', MarketId},
				   {'ExchangeId', ExchId}]} || #'P:EventType'{'id' = Id,
									      'name' = Name,
									      'nextMarketId' = MarketId,
									      'exchangeId' = ExchId} <- EventTypeItems]));
encode({all_markets, Bin}) ->
    L = parseMarkets(Bin, [], [], []),
    iolist_to_binary(
      mochijson2:encode([{struct, [{'MarketId', list_to_integer(MarketId)},
				   {'MarketName', list_to_binary(MarketName)},
				   {'MarketType', list_to_binary(MarketType)},
				   {'MarketStatus', list_to_binary(MarketStatus)},
				   {'EventDate', list_to_binary(EventDate)},
				   {'MenuPath', list_to_binary(MenuPath)},
				   {'EventHierarchy', list_to_binary(EventHierarchy)},
				   {'BetDelay', list_to_binary(BetDelay)},
				   {'ExchangeId', list_to_integer(ExchangeId)},
				   {'CountryISO3', list_to_binary(ISO3CountryCode)},
				   {'LastRefresh', list_to_binary(LastRefresh)},
				   {'NumberOfRunners', list_to_integer(NumberOfRunners)},
				   {'NumberOfWinners', list_to_integer(NumberOfWinners)},
				   {'TotalAmountMatched', list_to_float(TotalAmountMatched)},
				   {'BspMarket', list_to_binary(BSPMarket)},
				   {'TurningIntoPlay', list_to_binary(TurningIntoPlay)}]} ||
			    [MarketId, MarketName, MarketType, MarketStatus, EventDate, MenuPath, EventHierarchy,
			     BetDelay, ExchangeId, ISO3CountryCode, LastRefresh, NumberOfRunners, NumberOfWinners,
			     TotalAmountMatched, BSPMarket, TurningIntoPlay] <- L ]));


%% %%bf_json:encode({marketPrices, MP}).
%% encode({marketPrices, Bin}) ->

encode({marketPrices, Bin, Format}) ->
    [[[MarketId,
       Currency, 
       MarketStatus,
       InPlayDelay,
       NumberOfWinners,
       MarketInfo,
       DiscountAllowed,
       MarketBaseRate,
       RefreshTime,
       _RemovedRunners, %% TODO - parse removed runners
       BSPMarket  | _N]] | T] = parseMarketPrices(Bin), 
    
    case Format of
	default ->
	    iolist_to_binary(
	      mochijson2:encode({struct, [{'MarketId', list_to_integer(MarketId)},
					  {'Currency', list_to_binary(Currency)},
					  {'MarketStatus', list_to_binary(MarketStatus)},
					  {'InPlayDelay', list_to_integer(InPlayDelay)},
					  {'NumberOfWinners', list_to_integer(NumberOfWinners)},
					  {'MarketInfo', list_to_binary(MarketInfo)},
					  {'DiscountAllowed', list_to_atom(DiscountAllowed)},
					  {'MarketBaseRate', list_to_binary(MarketBaseRate)},
					  {'RefreshTime', list_to_integer(RefreshTime)},
					  {'BSPMarket', list_to_binary(BSPMarket)},
					  {'RunnersInfo', parseRunners(T, [])}
					 ]}));
	compressed ->
	    throw(compressed_format_not_implemented)
    end;

encode({bet, #'P:Bet'{'asianLineId' = AsianLineId,
		      'avgPrice' = AvgPrice,
		      'betId' = BetId,
		      'betStatus' = BetStatus,
		      'betType' = BetType,
		      'betCategoryType' = BetCategoryType,
		      'betPersistenceType' = BetPersistenceType,
		      'cancelledDate' = CancelledData,
		      'lapsedDate' = LapseDate,
		      'marketId' = MarketId,
		      'marketName' = MarketName,
		      'fullMarketName' = FullMarketName,
		      'marketType' = MarketType,
		      'marketTypeVariant' = MarketTypeVariant,
		      'matchedDate' = MatchedDate,
		      'matchedSize' = MatchedSize,
		      'matches' = #'P:ArrayOfMatch'{'Match' = Matches},
		      'placedDate' = PlacedDate,
		      'price' = Price,
		      'bspLiability' = BspLiability,
		      'profitAndLoss' = ProfitAndLoss,
		      'selectionId' = SelectionId,
		      'selectionName' = SelectionName,
		      'settledDate' = SettledDate,
		      'remainingSize' = RemainingSize,
		      'requestedSize' = RequestedSize,
		      %%'voidedDate' = voidedDate,
		      'handicap' = Handicap}}) ->     
    try
	iolist_to_binary(
	  mochijson2:encode({struct, [{'asianLineId', AsianLineId},
				      {'avgPrice', list_to_float(AvgPrice)},
				      {'betId', list_to_integer(BetId)},
				      {'betStatus', list_to_binary(BetStatus)},
				      {'betType', list_to_binary(BetType)},
				      {'betCategoryType', list_to_binary(BetCategoryType)}, 
				      {'betPersistenceType', list_to_binary(BetPersistenceType)},
				      {'cancelledDate', list_to_binary(CancelledData)},
				      {'lapsedDate', list_to_binary(LapseDate)},
				      {'marketId', MarketId},
				      {'marketName', list_to_binary(MarketName)},
				      {'fullMarketName', list_to_binary(FullMarketName)},
				      {'marketType', list_to_binary(MarketType)},
				      {'marketTypeVariant', list_to_binary(MarketTypeVariant)},
				      {'matchedDate', list_to_binary(MatchedDate)},
				      {'matchedSize', list_to_float(MatchedSize)},
				      {'matches', matches(Matches, [])},
				      {'placedDate', list_to_binary(PlacedDate)},
				      {'price', list_to_float(Price)},
				      {'bspLiability', list_to_float(BspLiability)},
 				      {'profitAndLoss', list_to_float(ProfitAndLoss)},
 				      {'selectionId', SelectionId},
				      {'selectionName', list_to_binary(SelectionName)},
				      {'settledDate', list_to_binary(SettledDate)},
				      {'remainingSize', list_to_float(RemainingSize)},
				      {'requestedSize', list_to_float(RequestedSize)},
 				      %%{'voidedDate', VoidedDate},
				      {'handicap', list_to_float(Handicap)}]}))
    catch
	_:_ -> {bf_json_encode_error}
    end;





encode(Other) ->
    throw({bf_json_error, {unknown_value, Other}}).
    %%Other.



%% Helper functions

%%--------------------------------------------------------------------
%% @doc
%% Parse binary string for Markets 
%% @end
%%--------------------------------------------------------------------
parseMarkets(<<":", Rest/binary>>, Field, Line, Acc) ->
    parseMarkets(Rest, [], [], [lists:reverse([lists:reverse(Field) | Line]) | Acc]);
%% parseMarkets(<<"\\:", Rest/binary>>, Field, Line, Acc) ->
%%     parseMarkets(Rest, [":" | Field], Line, Acc);
%% parseMarkets(<<"\\", Rest/binary>>, Field, Line, Acc) ->
%%     parseMarkets(Rest, [], [lists:reverse(Field) | Line], Acc);
parseMarkets(<<"~", Rest/binary>>, Field, Line, Acc) ->
    parseMarkets(Rest, [], [lists:reverse(Field) | Line], Acc);
parseMarkets(<<Char, Rest/binary>>, Field, Line, Acc) ->
    parseMarkets(Rest, [Char | Field], Line, Acc);
parseMarkets(<<>>, Field, Line, Acc) ->
    lists:delete([[]], lists:reverse([lists:reverse([lists:reverse(Field) | Line]) | Acc])).

runners([], R) -> R;
runners([ #'P:Runner'{'asianLineId' = AsianLineId, 'handicap' = Handicap, 'name' = Name, 'selectionId' = SelectionId} | T], R) ->
    runners(T, [{struct,[{'asianLineId', AsianLineId},
			 {'handicap', list_to_binary(Handicap)},
			 {'name', list_to_binary(Name)},
			 {'selectionId', SelectionId}]} | R]).

eventId(#'P:ArrayOfEventId'{'EventId' = EventId}) -> EventId.

matches([], M) -> M;
matches([#'P:Match'{'betStatus' = BetStatus,
		    'matchedDate' = MatchedDate,
		    'priceMatched' = PriceMatched,
		    'profitLoss' = ProfitLoss,
		    'settledDate' = SettledDate,
		    'sizeMatched' = SizeMatched,
		    'transactionId' = TransactionId,
		    'voidedDate' = VoidedDate
		   } | T], M) -> 
    matches(T, [{struct, [ {'betStatus', list_to_binary(BetStatus)},
			   {'matchedDate', list_to_binary(MatchedDate)},
			   {'priceMatched', list_to_float(PriceMatched)}, 
			   {'profitLoss', list_to_float(ProfitLoss)},
			   {'settledDate', list_to_binary(SettledDate)},
			   {'sizeMatched', list_to_float(SizeMatched)},
			   {'transactionId', list_to_integer(TransactionId)},
			   {'voidedDate', list_to_binary(VoidedDate)}
			 ]} | M]).


parseMarketPrices(Bin) ->
    parseMarketPrices(Bin, [], [], [], []).
parseMarketPrices(<<"~", Rest/binary>>, Field, Line, Lines, Acc) ->
    parseMarketPrices(Rest, [], [lists:reverse(Field) | Line], Lines, Acc);
parseMarketPrices(<<":", Rest/binary>>, Field, Line, Lines, Acc) ->
    parseMarketPrices(Rest, [], [], [], [lists:reverse([lists:reverse([lists:reverse(Field) | Line]) | Lines]) | Acc]);
parseMarketPrices(<<"|", Rest/binary>>, Field, Line, Lines, Acc) ->
    parseMarketPrices(Rest, [], [], [lists:reverse([lists:reverse(Field) | Line]) | Lines], Acc);
parseMarketPrices(<<Char, Rest/binary>>, Field, Line, Lines, Acc) ->
    parseMarketPrices(Rest, [Char | Field], Line, Lines, Acc);
parseMarketPrices(<<>>, Field, Line, Lines, Acc) ->
    lists:reverse([lists:reverse([lists:reverse([lists:reverse(Field) | Line]) | Lines]) | Acc]).


parseRunners([], Acc ) ->  Acc;
parseRunners([[[SelectionId,
	       OrderIndex,
	       TotalAmountMatched,
	       LastPriceMatched,
	       Handicap,
	       ReductionFactor,
	       Vacant,
	       FarSPPrice,
	       NearSPPrice,
	       ActualSPPrice | _N] | Prices] | T], Acc) ->
    %io:format("prices ~p~n", [Prices]),
    parseRunners(T, [{struct, [ {'SelectionId', list_to_integer(SelectionId)},
				{'OrderIndex', list_to_integer(OrderIndex)},
				{'TotalAmountMatched', list_to_binary(TotalAmountMatched)}, 
				{'LastPriceMatched', list_to_binary(LastPriceMatched)},
				{'Handicap', list_to_binary(Handicap)},
				{'ReductionFactor', list_to_binary(ReductionFactor)},
				{'Vacant', list_to_atom(Vacant)},
				{'FarSPPrice', list_to_binary(FarSPPrice)},
				{'NearSPPrice', list_to_binary(NearSPPrice)},
				{'ActualSPPrice', list_to_binary(ActualSPPrice)},
				{'Prices', parsePrices(Prices, [])}
			      ]} | Acc]).

parsePrices([], Acc) -> Acc;
parsePrices([H | T], Acc) -> 
    Split = split(lists:delete([],H), []),
    %io:format("split ~p~n", [Split]),
    parsePrices( T, [[{struct, [{'price', list_to_float(Price)},
				{'amount', list_to_float(AmountAvailable)},
				{'side', list_to_binary(Side)},
				{'depth', list_to_integer(Depth)}]} || [Price, AmountAvailable, Side, Depth] <- Split] | Acc]).


split([], Acc) -> lists:reverse(Acc);
split(L, Acc) ->
    {H, T} = lists:split(4, L),
    split(T, [H | Acc]).
    
