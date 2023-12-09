:- use_module(library(dcg_tools)).
:- use_module(library(medikit)).

nums(Ns)    --> list(integer, sp, Ns), nl.
input(File) --> list(nums, File).

list_init_last(Xs, Init, Last) :-
    append([Init, [Last]], Xs).

list_head_tail(Xs, First, Tail) :-
    Xs = [First|Tail].

list_first_rest_last(List, First, Rest, Last) :-
    append([[First], Rest, [Last]], List).

differences(Xs, Ds) :-
    list_init_last(Xs, XInit, _),
    list_head_tail(Xs, _, XTail),
    maplist([A,B,C]>>(C#=B-A), XInit, XTail, Ds).

differences2(Xs, Zs) :-
    findall(Z, (nextto(X, Y, Xs), Z #= Y-X), Zs).

follow(Xs) :- once((differences(Xs, Ys), (maplist(=(0), Ys) ; follow(Ys)))).
follow2(Xs) :- differences2(Xs, Ys), (maplist(=(0), Ys) ; follow2(Ys)).

solution(Part1, Part2) :-
    once(phrase_from_file(input(Nss), 'input1.data')),
    maplist(list_first_rest_last, C, Prefixes, Nss, Postfixes),
    maplist(follow, C),
    list_sum(Postfixes, Part1),
    list_sum(Prefixes, Part2).

