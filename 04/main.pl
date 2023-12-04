:- use_module(library(dcg_tools)).
:- use_module(library(medikit)).

:- dynamic card_matches/2.
input(Lines) --> list(line, Lines).
line(card_matches(CardID, N)) -->
    `Card`, sp, natural(CardID), `:`, sp,
    list(natural, sp, Winning), sp, `|`, sp,
    list(natural, sp, Available), nl,
    {intersection(Winning, Available, I), length(I, N)}.

parse(File) :-
    once(phrase_from_file(input(Knowledge), File)),
    set_knowledge(Knowledge, [card_matches(_,_)]).

% Part 1
prize(0, 0).
prize(N, M) :- N #> 0, M #= 2^ξ(N-1).

part1(Sol) :-
    aggregate_all(sum(Prize), (card_matches(_, Matches), prize(Matches,Prize)), Sol).

% Part 2
:- table card_copies/2.
card_copies(CardId, Copies) :-
    aggregate_all(sum(NC), (card_matches(C,M), C #< CardId, CardId #=< ξ(C+M), card_copies(C, NC)), ξ(Copies-1)).

part2(Sol) :-
    aggregate_all(sum(Copies), (card_matches(C, _), card_copies(C, Copies)), Sol).
