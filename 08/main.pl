:- use_module(library(dcg_tools)).
:- use_module(library(medikit)).

:- dynamic (instructions/1, direction/3).

instr(instructions(Xs)) --> list(letter, Xs), nl, nl.
label(X) --> list(digit|letter)//list(any, C), {atom_codes(X, C)}.
dir(direction('L',Origin,L)-direction('R', Origin, R)) --> label(Origin), " = (", label(L), ", ", label(R), ")", nl.
input(Xs-ML) --> instr(Xs), list(dir, ML).

parse :-
    once(phrase_from_file(input(Knowledge), 'input2.data')),
    set_knowledge(Knowledge, [instructions(_), direction(_,_,_)]).

follow(N, N, _, StopCondition, Stop) :-
    call(StopCondition, Stop).
follow(N, NF, [I|Instructions], StopCondition, Current) :-
    N1 is N+1, direction(I, Current, Next),
    follow(N1, NF, Instructions, StopCondition, Next).

solutions(Part1, Part2) :-
    instructions(Instr), append(Instr, RepeatedInstr, RepeatedInstr),
    once(follow(0, Part1, RepeatedInstr, =('ZZZ'), 'AAA')),
    aggregate_all(set(O), (direction(_, O, _), atom_concat(_, 'A', O)), Starts),
    maplist({RepeatedInstr}/[S,L]>>once(follow(0, L, RepeatedInstr, atom_concat(_, 'Z'), S)), Starts, Lens),
    list_lcm(Lens, Part2).

