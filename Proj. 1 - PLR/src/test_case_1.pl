% ------- TEST CASE 1 ---------

% ---------
% Factories
%
% test_factories([MaxCapacityPerDay,...])
test_factories([20,30,20,30,50]).
%
test_factories_production_cost(
        [[31,45,38],
        [29,41,45],
        [0,46,40],
        [28,42,0],
        [29,43,0]]).

% --------------
% Expected Sales
%
% test_sales(Period, [DailySalesPerFactory])
%
test_sales(1,[10,40,30]).
test_sales(2,[20,20,10]).
test_sales(3,[40,40,60]).

% -------------------------
% Hour-related restrictions
%
% test_hours_per_product <-- each product's manufacturing time
% test_hours_per_day <-- available daily work hours (per factory)
%
test_hours_per_product([2,3,2]).
test_hours_per_day(120).

% --------------------------------------
% Warehouse/Storage related restrictions
%
% test_stock <-- available stock at the beginning of the year
% test_min_storage <-- minimum storage required, per product, at the end of each period
%
test_stock(10).
test_min_storage(15).

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
cannot_produce(1, [[3,1],[4,3],[5,3]]).
cannot_produce(2, [[3,1],[4,3],[5,3]]).
cannot_produce(3, [[1,1],[2,1],[3,1],[4,1],[5,1],[4,3],[5,3]]).

% ------------------------------------------------------------
% Proportions in which products are manufactured (per factory)
%
% Each parcel: [FactoryIndex, Product1, Product2, ProportionFactor]
% 
% This restriction will be translated in:
%
% Product1 #= (ProportionFactor * Product2)
%
produce_proportion([[2,1,2,2],[2,1,3,3],[4,2,1,2],[5,2,1,2]]).