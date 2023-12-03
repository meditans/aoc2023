:- use_module(library(dcg_tools)).
:- use_module(library(medikit)).
:- use_module(library(chr)).

% Parsing
dgt(dgt(N)) --> digit(N).
sym(sym(S)) --> symbol(S), {S \= '.'}.
dot(dot)    --> `.`.
row(Xs)     --> list((dgt | sym | dot), Xs), nl.
input(Xs)   --> list(row, Xs).

position(M, I-J, X) :- nth0(I, M, Row), nth0(J, Row, X).

:- chr_constraint n/3.
n(I-J1, L1, N1), n(I-J2, L2, N2) <=> J1 + L1 #= J2 |
                                     n(I-J1, ξ(L1 + L2), ξ(N1*10^L2 + N2)).

parse(File, Mat-Ns) :-
    once(phrase_from_file(input(Mat), File)),
    findall(n(I-J, 1, N), position(Mat, I-J, dgt(N)), Constraints),
    local_chr(Constraints, n(_, _, _), Ns).

% Part 1
adjacent(I-J, n(A-B, L, _)) :-
    I in ξ(A-1)..ξ(A+1),
    J in ξ(B-1)..ξ(B+L).

partNumber(Mat-Ns, A-B, Value) :-
    position(Mat, I-J, sym(_)),
    member(n(A-B, L, Value), Ns),
    adjacent(I-J, n(A-B, L, Value)).

part1(Sol) :-
    parse('input1.data', Mat-Ns),
    writeln('parsed'),
    aggregate(sum(N), I-J, partNumber(Mat-Ns, I-J, N), Sol).

% Part 2
gear(Mat-Ns, I-J, Ratio) :-
    position(Mat, I-J, sym('*')),
    include(adjacent(I-J), Ns, [n(_,_,N1), n(_,_,N2)]),
    Ratio #= N1*N2.

part2(Sol) :-
    parse('input1.data', Mat-Ns),
    aggregate(sum(Ratio), I-J, gear(Mat-Ns, I-J, Ratio), Sol).
