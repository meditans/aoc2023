:- use_module(library(dcg_tools)).
:- use_module(library(medikit)).
:- use_module(library(chr)).


% Parsing grammar
bunch(bunch(N,Color)) --> natural(N), ` `, word(Color).
gameset(Bunches)      --> list(bunch, constant(`, `), Bunches).
game(N-Gamesets)      --> `Game `, natural(N), `: `, list(gameset, constant(`; `), Gamesets), nl.
input(Games)          --> list(game, Games).

% Part 1
avoid(bunch(N, red))   :- N > 12.
avoid(bunch(N, green)) :- N > 13.
avoid(bunch(N, blue))  :- N > 14.

part1(Solution) :-
    once(phrase_from_file(input(Games), 'input1.data')),
    convlist([GameId-Gamesets, GameId]>>subterms(Gamesets, avoid, []), Games, GoodGameIds),
    list_sum(GoodGameIds, Solution).

% Part 2
:- chr_constraint bunch/2.
bunch(N, Color), bunch(M, Color) <=> M =< N | bunch(N, Color).

game_power(G, Power) :-
    subterms(G, subsumes_term(bunch(_,_)), Constraints),
    local_chr(Constraints, bunch(_,_), Results),
    subterms(Results, number, Ns),
    list_product(Ns, Power).

part2(Solution) :-
    once(phrase_from_file(input(Games), 'input1.data')),
    maplist(game_power, Games, Powers),
    list_sum(Powers, Solution).
