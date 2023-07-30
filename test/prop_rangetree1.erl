-module(prop_rangetree1).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_all_covered_in_range() ->
    ?FORALL({Points, {Min, Max}}, {points(), range()},
        begin
            T = rangetree1:new(Points),
            Covered = rangetree1:query(Min, Max, T),
            {CoveredValues, _NotCoveredValues} = partition_values(Points, Covered),
            lists:all(fun(X) -> (Min =< X) and (X =< Max) end, CoveredValues)
        end).

prop_all_not_covered_out_of_range() ->
    ?FORALL({Points, {Min, Max}}, {non_empty(list(float())), range()},
        begin
            T = rangetree1:new(Points),
            Covered = rangetree1:query(Min, Max, T),
            {_CoveredValues, NotCoveredValues} = partition_values(Points, Covered),
            lists:all(fun(X) -> (X > Max) or (X < Min) end, NotCoveredValues)
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
partition_values(Points, Covered) ->
    {[lists:nth(N, Points) || N <- Covered],
     [lists:nth(N, Points) || N <- (lists:seq(1, length(Points)) -- Covered)]}.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
points() ->
    non_empty(list(float())).

range() ->
    ?SUCHTHAT({Min, Max}, {float(), float()}, Min =< Max).
