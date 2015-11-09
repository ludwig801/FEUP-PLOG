% ------- TEST CASE 1 ---------

% ---------
% Factories
%
% test_factories([MaxCapacityPerDay,...])
test_factories([20,30,40]).
%
test_factories_production_cost(
        [[0,31],
        [28,20],
        [4,30]]).

% --------------
% Expected Sales
%
% test_sales(Period, [DailySalesPerFactory])
%
test_sales(1,[20,10]).
test_sales(2,[20,40]).
test_sales(3,[40,5]).

% -------------------------
% Hour-related restrictions
%
% test_hours_per_product <-- each product's manufacturing time
% test_hours_per_day <-- available daily work hours (per factory)
%
test_hours_per_product([2,4]).
test_hours_per_day(100).

% --------------------------------------
% Warehouse/Storage related restrictions
%
% test_stock <-- available stock at the beginning of the year
% test_min_storage <-- minimum storage required, per product, at the end of each period
%
test_stock(10).
test_min_storage(100).

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
cannot_produce(1, [[1,1]]).
cannot_produce(2, [[1,1]]).
cannot_produce(3, [[1,1]]).

% ------------------------------------------------------------
% Proportions in which products are manufactured (per factory)
%
% Each parcel: [FactoryIndex, Product1, Product2, ProportionFactor]
% 
% This restriction will be translated in:
%
% Product1 #= (ProportionFactor * Product2)
%
produce_proportion([[3,1,2,2]]).