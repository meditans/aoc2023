:- use_module(library(dcg_tools)).
:- use_module(library(medikit)).
:- use_module(library(clpfd)).

:- dynamic race/2, big_race/2.

times(Times) --> "Time:",     sp, list(natural, sp, Times).
dists(Dists) --> "Distance:", sp, list(natural, sp, Dists).
input(K1-K2) --> times(Times), nl, dists(Dists), nl,
                 { maplist_op(race, Times, Dists, K1), K2 = big_race(BigTime, BigDist),
                   atomic_list_concat(Times, BigTime0), atom_number(BigTime0, BigTime),
                   atomic_list_concat(Dists, BigDist0), atom_number(BigDist0, BigDist) }.

win(MaxTime, Record, NWays) :-
    Charge in 0..MaxTime,
    Distance #= Charge * (MaxTime - Charge),
    Distance #> Record,
    fd_size(Charge, NWays).

solution(Part1, Part2) :-
    once(phrase_from_file(input(X), 'input1.data')),
    set_knowledge(X, [race(_,_), big_race(_,_)]),
    aggregate_all(set(Ways), (race(T,D), win(T,D,Ways)), Res),
    list_product(Res, Part1),
    big_race(T, D), win(T, D, Part2).
