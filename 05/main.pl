:- use_module(library(dcg_tools)).
:- use_module(library(medikit)).

:- dynamic transformation/4, seeds/1, seed_range/1.

input(Seeds-Maps) --> seeds(Seeds), list(map, nl, Maps).
seeds(seeds(Seeds)-Ranges) -->
    "seeds:", sp, list(seed_range, sp, Ranges)//list(natural, sp, Seeds), nl, nl.
seed_range(seed_range(A..B)) -->
    natural(A), sp, natural(L), {B #= A + L}.
map(Knowledge) -->
    word(From), "-to-", word(To), " map:\n", list(part(From, To), Knowledge).
part(From, To, transformation(From, To, Range, Diff)) -->
    natural(A), sp, natural(B), sp, natural(L), nl, {Range = B..ξ(B+L-1), Diff #= A-B}.

parse(File) :-
    once(phrase_from_file(input(Knowledge), File)),
    set_knowledge(Knowledge, [seeds(_), seed_range(_), transformation(_,_,_,_)]).

starting_seeds(Part1, Part2) :-
    seeds(Seeds), fold_op(\/, Seeds, Part1),
    aggregate_all(set(I), seed_range(I), SeedRanges),
    domain_union(SeedRanges, Part2).

transmute(S0, S7) :-
    transform(seed, soil, S0, S1),         transform(soil, fertilizer, S1, S2),
    transform(fertilizer, water, S2, S3),  transform(water, light, S3, S4),
    transform(light, temperature, S4, S5), transform(temperature, humidity, S5, S6),
    transform(humidity, location, S6, S7).

solution(Part1, Part2) :-
    parse('input1.data'), starting_seeds(S1, S2),
    transmute(S1, R1), X in R1, fd_inf(X, Part1),
    transmute(S2, R2), Y in R2, fd_inf(Y, Part2).

transform_part(Thing, Another, Dom0, Dom1) :-
    transformation(Thing, Another, Interval, Diff),
    X in Interval, X in Dom0, Y #= X + Diff, fd_dom(Y, Dom1).
transform_part(Thing, Another, Dom0, Dom1) :-
    foreach(transformation(Thing, Another, I, _), #\ N in I),
    N in Dom0, fd_dom(N, Dom1).

transform(Thing, Another, Dom0, Dom1) :-
    aggregate_all(set(Part), transform_part(Thing, Another, Dom0, Part), Parts),
    domain_union(Parts, Dom1).

domain_union(Domains, Domain) :-
    fold_op(\/, Domains, Constraint), X in Constraint, fd_dom(X, Domain).
