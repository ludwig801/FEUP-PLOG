% This will be the one, i'm sure of it
%
%   My own... my Prrrrrrecious...
%
%
%
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(timeout)).


% uncomment any of the below to test diferent test cases
%   (do not forget, only ONE of the consults can be active)
%
%:- consult(test_case_1).
%:- consult(test_case_2).
:- consult(test_case_3).

%   test_restrictions(Period, PPF, PPP)
restrictions_production(Period, ProductionPerFactory) :-
        cannot_produce(Period, Restrictions),
        restrictions_cannot_produce(Restrictions, ProductionPerFactory),
        produce_proportion(Proportions),
        restrictions_produce_proportion(Proportions, ProductionPerFactory).

restrictions_cannot_produce([],_).
restrictions_cannot_produce([[FactoryIndex,ProductIndex]|Rs],ProductionPerFactory) :-
        nth1(FactoryIndex, ProductionPerFactory, Factory),
        element(ProductIndex, Factory, Product),
        Product #= 0,
        restrictions_cannot_produce(Rs,ProductionPerFactory).

restrictions_produce_proportion([],_).
restrictions_produce_proportion([[FactoryIndex, I1, I2, ProportionFactor]|Ps], ProductionPerFactory) :-
        nth1(FactoryIndex, ProductionPerFactory, Factory),
        element(I1, Factory, Product1),
        element(I2, Factory, Product2),
        Product1 #= (ProportionFactor * Product2),
        restrictions_produce_proportion(Ps, ProductionPerFactory).

restrictions_storage(_,[],[],[],_).
restrictions_storage([ProductionPerProduct|Ps], [Sales|Ss], [OldStorage|Xs], [NewStorage|Ys], DaysPerPeriod) :-
        sum(ProductionPerProduct, #=, ProductProduction),
        NewStorage #= ((ProductProduction - Sales) * DaysPerPeriod + OldStorage),
        restrictions_storage(Ps, Ss, Xs, Ys, DaysPerPeriod).

restrictions_hours_per_factory([],[],0).
restrictions_hours_per_factory([ProductPerFactory|Ps],[ProductWorkHour|Hs], Result) :-
        restrictions_hours_per_factory(Ps,Hs, Result1),
        Result #= (Result1 + (ProductPerFactory * ProductWorkHour)).
 
restrictions_hours([],_,_).
restrictions_hours([ProductionPerFactory|Fs], HoursPerProduct, [Workhours|Ws]) :-
        restrictions_hours_per_factory(ProductionPerFactory, HoursPerProduct, Workhours),
        restrictions_hours(Fs, HoursPerProduct, Ws).

restrictions_cost_per_product([],[],[]).
restrictions_cost_per_product([Product|Ps],[ProductCostPerFactory|Cs],[ProductCost|Rs]) :-
        ProductCostPerFactory #= (Product * ProductCost),
        restrictions_cost_per_product(Ps,Cs,Rs).

restrictions_cost([],[],[]).
restrictions_cost([ProductionPerFactory|Ps], [ProductionCostPerFactory|Cs], [ProductCostPerFactory|Rs]) :-
        restrictions_cost_per_product(ProductionPerFactory,ProductionCostPerFactory,ProductCostPerFactory),
        restrictions_cost(Ps,Cs,Rs).

init(Factories, HoursPerProduct, Stock, MinStorage, DailyHours,
     ProductionPerFactory1, ProductionPerFactory2, ProductionPerFactory3,
     ProductionCostFactory1, ProductionCostFactory2, ProductionCostFactory3,
     ProductionPerProduct1, ProductionPerProduct2, ProductionPerProduct3,
     Storage1, Storage2, Storage3,
     Sales1, Sales2, Sales3,
     WorkHoursPerFactory1, WorkHoursPerFactory2, WorkHoursPerFactory3,
     FirstStorage) :-
        
        % get the number of products and factories
        length(HoursPerProduct, NumProducts),
        length(Factories, NumFactories),
        %
        length(ProductionPerFactory1, NumFactories),
        length(ProductionPerFactory2, NumFactories),
        length(ProductionPerFactory3, NumFactories),
        %
        create_production_per_factory(ProductionPerFactory1, Factories, NumProducts),
        create_production_per_factory(ProductionPerFactory2, Factories, NumProducts),
        create_production_per_factory(ProductionPerFactory3, Factories, NumProducts),
        %
        length(ProductionCostFactory1, NumFactories),
        length(ProductionCostFactory2, NumFactories),
        length(ProductionCostFactory3, NumFactories),
        %
        create_cost_per_factory(ProductionCostFactory1, NumProducts),
        create_cost_per_factory(ProductionCostFactory1, NumProducts),
        create_cost_per_factory(ProductionCostFactory1, NumProducts),
        %
        transpose(ProductionPerFactory1, ProductionPerProduct1),
        transpose(ProductionPerFactory2, ProductionPerProduct2),
        transpose(ProductionPerFactory3, ProductionPerProduct3),
        % ------------------------------
        % creates the first storage list
        % (this list contains the items stored at
        %   the beggining of the year in the warehouse)
        length(FirstStorage, NumProducts),
        %
        fill_list(FirstStorage, Stock),
        % -------------------------------------------------------
        % creates the warehouse storage at the end of each period
        length(Storage1, NumProducts),
        length(Storage2, NumProducts),
        length(Storage3, NumProducts),
        %
        domain(Storage1, MinStorage, sup),
        domain(Storage2, MinStorage, sup),
        domain(Storage3, MinStorage, sup),
        % -----------------------------------------------------------
        % creates a list of production hours per factory, period-wise
        length(WorkHoursPerFactory1, NumFactories),
        length(WorkHoursPerFactory2, NumFactories),
        length(WorkHoursPerFactory3, NumFactories),
        %
        domain(WorkHoursPerFactory1, 0, DailyHours),
        domain(WorkHoursPerFactory2, 0, DailyHours),
        domain(WorkHoursPerFactory3, 0, DailyHours),
        % -------------------------------------------------------------------
        % creates the lists with the expected sales each period (daily vends)
        test_sales(1,Sales1),
        test_sales(2,Sales2),
        test_sales(3,Sales3).

create_production_per_factory([],[],_).
create_production_per_factory([Production|Ps],[MaxCapacity|Fs],NumProducts) :-
        length(Production, NumProducts),
        sum(Production, #=<, MaxCapacity),
        domain(Production, 0, MaxCapacity),
        create_production_per_factory(Ps,Fs,NumProducts).

create_cost_per_factory([],_).
create_cost_per_factory([P|Ps],NumProducts) :-
        length(P, NumProducts),
        domain(P, 0, sup),
        create_cost_per_factory(Ps, NumProducts).

% fills a list with a certain value. Note that this method DOES unify that value
fill_list([],_).
fill_list([X|Xs], Value) :-
        X is Value,
        fill_list(Xs, Value).

get_comulative_matrix([],[]).
get_comulative_matrix([Array|As],[Comulate|Cs]) :-
        sumlist(Array, X),
        test_days_per_period(Days),
        Comulate is (X * Days),
        get_comulative_matrix(As,Cs).

get_comulative([],[]).
get_comulative([X|Xs],[Comulate|Cs]) :-
        test_days_per_period(Days),
        Comulate is (X * Days),
        get_comulative(Xs,Cs).

print_2(Period, PPF, PCPF, PPP, OST, ST, SA, WH) :-
        write('> Period '), write(Period), nl,
        write('  > Production Per Factory'), nl, write('    '), write(PPF), nl,
        write('  > Production Cost Per Factory'), nl, write('    '), write(PCPF), nl,
        write('  > Initial Storage'), nl, write('    '), write(OST), nl,
        write('  > Production Per Product'), nl, write('    '), write(PPP), nl,
        length(PPP, L1), length(C, L1), get_comulative_matrix(PPP, C),
        write('    '), write(C), nl,
        write('  > Sales Per Product'), nl,
        length(SA, L2), length(S, L2), get_comulative(SA, S),
        write('    '), write(S), nl,
        write('  > Storage at the end of the period'), nl, write('    '), write(ST), nl,
        write('  > Workhours Per Factory'), nl, write('    '), write(WH), nl,
        nl.

print_1(Factories, FactoriesProductionCost, HoursPerProduct, InitialStock, MinStorage, DailyHours, DaysPerPeriod) :-
        write('> Factories Max Capacity'), nl,
        write('    '), write(Factories), nl,
        write('> Factories Prodution Costs'), nl,
        write('    '), write(FactoriesProductionCost), nl,
        write('> Hours Per Product'), nl,
        write('    '), write(HoursPerProduct), nl,
        write('> Initial Stock'), nl,
        write('    '), write(InitialStock), nl,
        write('> Min. Storage'), nl,
        write('    '), write(MinStorage), nl,
        write('> Daily Hours'), nl,
        write('    '), write(DailyHours), nl,
        write('> Days Per Period'), nl,
        write('    '), write(DaysPerPeriod), nl,
        nl.

solve :-
        test_factories(Factories),
        test_factories_production_cost(FactoriesProductionCost),
        test_hours_per_product(HoursPerProduct),
        test_stock(InitialStock),
        test_min_storage(MinStorage),
        test_hours_per_day(DailyHours),
        test_days_per_period(DaysPerPeriod),
        %test_cost_per_unit(CostPerUnit),
        print_1(Factories, FactoriesProductionCost, HoursPerProduct, InitialStock, MinStorage, DailyHours, DaysPerPeriod),
        solve(Factories, HoursPerProduct, FactoriesProductionCost, DailyHours, DaysPerPeriod, InitialStock, MinStorage).

       
solve(Factories, HoursPerProduct, FactoriesProductionCost, DailyHours, DaysPerPeriod, Stock, MinStorage) :-
        init(Factories, HoursPerProduct, Stock, MinStorage, DailyHours,
             ProductionPerFactory1, ProductionPerFactory2, ProductionPerFactory3,
             ProductionCostFactory1, ProductionCostFactory2, ProductionCostFactory3,
             ProductionPerProduct1, ProductionPerProduct2, ProductionPerProduct3,
             Storage1, Storage2, Storage3,
             Sales1, Sales2, Sales3,
             WorkHoursPerFactory1, WorkHoursPerFactory2, WorkHoursPerFactory3,
             Storage0),
              
        restrictions_production(1, ProductionPerFactory1),
        restrictions_production(2, ProductionPerFactory2),
        restrictions_production(3, ProductionPerFactory3),
        
        restrictions_storage(ProductionPerProduct1, Sales1, Storage0, Storage1, DaysPerPeriod),
        restrictions_storage(ProductionPerProduct2, Sales2, Storage1, Storage2, DaysPerPeriod),
        restrictions_storage(ProductionPerProduct3, Sales3, Storage2, Storage3, DaysPerPeriod),
        
        restrictions_hours(ProductionPerFactory1, HoursPerProduct, WorkHoursPerFactory1),
        restrictions_hours(ProductionPerFactory2, HoursPerProduct, WorkHoursPerFactory2),
        restrictions_hours(ProductionPerFactory3, HoursPerProduct, WorkHoursPerFactory3),
        
        restrictions_cost(ProductionPerFactory1, ProductionCostFactory1, FactoriesProductionCost),
        restrictions_cost(ProductionPerFactory2, ProductionCostFactory2, FactoriesProductionCost),
        restrictions_cost(ProductionPerFactory3, ProductionCostFactory3, FactoriesProductionCost),

        % cria a lista de producao (objetivo do labeling)
        append(ProductionPerFactory1, P1),
        append(ProductionPerFactory2, P2),
        append(ProductionPerFactory3, P3),
        append(P1, P2, P12),
        append(P12, P3, Res),
        
        % cria a lista de producao (objetivo do labeling)
        append(ProductionCostFactory1, C1),
        append(ProductionCostFactory2, C2),
        append(ProductionCostFactory3, C3),
        
        sum(C1, #=, Cost1),
        sum(C2, #=, Cost2),
        sum(C3, #=, Cost3),
        
        %sum(Storage1, #=, CostStorage1),
        %sum(Storage2, #=, CostStorage2),
        %sum(Storage3, #=, CostStorage3),
        
        Cost #= (Cost1 + Cost2 + Cost3),

        labeling([time_out(2000,_),minimize(Cost)],Res), % <-- 2sec
        %labeling([time_out(60000,_),minimize(Cost)],Res), % <-- 1min
        %labeling([time_out(120000,_),minimize(Cost)],Res), % <-- 2min
        %labeling([time_out(600000,_),minimize(Cost)],Res), % <-- 10min
        %labeling([time_out(1800000,_),minimize(Cost)],Res), % <-- 30min
        %labeling([time_out(3600000,_),minimize(Cost)],Res), % <-- 60min
        %labeling([time_out(120000,_)], Res),
        
        print_2(1, ProductionPerFactory1, ProductionCostFactory1, ProductionPerProduct1, Storage0, Storage1, Sales1, WorkHoursPerFactory1),
        print_2(2, ProductionPerFactory2, ProductionCostFactory2, ProductionPerProduct2, Storage1, Storage2, Sales2, WorkHoursPerFactory2),
        print_2(3, ProductionPerFactory3, ProductionCostFactory3, ProductionPerProduct3, Storage2, Storage3, Sales3, WorkHoursPerFactory3),
        write('Total Cost: '), write(Cost), nl,
        write('').
        
