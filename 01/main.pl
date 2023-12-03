:- use_module(library(dcg_tools)).

% If you want part 1, remove the line below
num(N) --> {member(N-Spelling, [1-`one`, 2-`two`, 3-`three`, 4-`four`, 5-`five`, 6-`six`, 7-`seven`, 8-`eight`, 9-`nine`])}, Spelling.
num(N) --> digit(N).
low(N) --> lowercase(N).

line(First-Last) --> non_greedy(low), num(First), (list(num | low), num(Last) | {Last = First}), list(low), nl.
input(Lines)     --> list(line, Lines).

part1or2(Total) :-
    once(phrase_from_file(input(Lines), 'input1.data')),
    maplist([First-Last, X] >> (X is 10*First + Last), Lines, LineResults),
    sum_list(LineResults, Total).
