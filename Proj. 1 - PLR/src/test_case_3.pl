% ------- TEST CASE 1 ---------

% ---------
% Factories
%
% test_factories([MaxCapacityPerDay,...])
test_factories([120,120]).
%
test_factories_production_cost(
        [[40,20,10,0],
        [40,19,40,32]]).

% --------------
% Expected Sales
%
% test_sales(Period, [DailySalesPerFactory])
%
test_sales(1,[10,20,40,10]).
test_sales(2,[30,20,30,30]).
test_sales(3,[40,10,10,40]).

% -------------------------
% Hour-related restrictions
%
% test_hours_per_product <-- each product's manufacturing time
% test_hours_per_day <-- available daily work hours (per factory)
%
test_hours_per_product([4,1,1,2]).
test_hours_per_day(180).

% --------------------------------------
% Warehouse/Storage related restrictions
%
% test_stock <-- available stock at the beginning of the year
% test_min_storage <-- minimum storage required, per product, at the end of each period
%
test_stock(20).
test_min_storage(10).

% ------------------------
test_days_per_period(120).

% -------------------------------
% paramaterizable restrictions:

% ------------------------------------------------------------
% Items that cannot be produced by factories in a given period
%
% (contains also the restriction that, in the 3rd period,
%   product A - in the problem sheet - could not be produced)
%
% cannot_produce(Period, [[FactoryIndex,ProductIndex],...])        
cannot_produce(1, [[1,4]]).
cannot_produce(2, [[1,4],[2,4]]).
cannot_produce(3, [[1,4],[1,1],[2,1]]).

% ------------------------------------------------------------
% Proportions in which products are manufactured (per factory)
%
% Each parcel: [FactoryIndex, Product1, Product2, ProportionFactor]
% 
% This restriction will be translated in:
%
% Product1 #= (ProportionFactor * Product2)
%
produce_proportion([[1,1,3,2],[2,2,3,2]]).