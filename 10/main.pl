:- use_module(library(dcg_tools)).
:- use_module(library(medikit)).

% During the solution, we will accumulate knowledge of the following facts:
%
% p(I-J, S), means that at the location I-J in the matrix, we observed a symbol S.
%
% side(Side) is the dimension of the input (which is a square).
%
% in_loop(P), where P has the form p/2, denotes that P is part of the loop.
:- dynamic (p/2, side/1, in_loop/1).

solution(Part1, Part2) :-
    % We gather knowledge from the input
    phrase_from_file(input(MatrixKnowledge), 'input1.data'),
    set_knowledge(MatrixKnowledge, [p(_,_), side(_)]),

    % We gather information about the loop
    phrase(loop, Loop),
    set_knowledge(Loop, [in_loop(_)]),

    % Part 1: we count how many elements we have, and divide by half
    aggregate_all(count, in_loop(_), ξ(2*Part1)),

    % Part 2: we sum for each line, the internal points in each line
    side(Side),
    aggregate_all(sum(Inside), (between(1, Side, Line), line_inside(Line, Inside)), Part2).

% Matrix parsing
%
% sym(S) denotes a symbol that can appear in our problem.
%
% line(S) denotes a list of symbols.
%
% input(side(Side)-MatrixPositions) captures the knowledge we have about the
% input matrix: its size and the position and symbols of the elements.
sym(S)   --> [C], { member(C, `|-FLJ7S.`), atom_codes(S, [C]) }.
line(Symbols) --> list(sym, Symbols), nl.
input(side(Side)-MatrixPositions) -->
    list(line, Matrix),
    { length(Matrix, Side), findall(p(I-J, X), position(Matrix, I-J, X), MatrixPositions) }.

position(M, I-J, X) :- nth1(I, M, Row), nth1(J, Row, X).

% Detecting loops
%
% Here we find the loop, we substitute the starting symbol with the symbol that
% is being replaced, and produce the knowledge about which positions are in the
% loop (via the in_loop predicates that it describes)
%
% loop describes a list [in_loop(A1), in_loop(A2)...], where every A is in the
% p/2 form. We impose a relation between the source and a first element, then we
% write the source, and continue with the first element.
loop -->
    { p(SourcePos, 'S'), Source = p(SourcePos, _), path_triple(_, Source, First), call(First) },
    [ in_loop(Source) ], continue_loop(Source, Source, First).

% continue_loop(Source, Current, Next) describes a list of consecutive pipes,
% that originally started with Source (we need to keep this information to
% determine the source symbol when we complete the loop), the current and the
% next step.
%
% We can extend our path by another step, we just use path_triple to see where
% to go.
continue_loop(Source, P1, P2) -->
    { path_triple(P1, P2, P3), call(P3) },
    [ in_loop(P2) ], continue_loop(Source, P2, P3).
% If the next step is at the source position we are done! We determine the true
% symbol of the source using path_triple/3.
continue_loop(p(SourcePos, SourceSym), Last, p(SourcePos, 'S')) -->
    { path_triple(Last, p(SourcePos, SourceSym), _) }, [].

% path_triple(A,B,C) is true if A,B,C are of the form p/2 and the symbol in B is
% compatible with the positions of A and C.
path_triple(p(Pos1, _), Middle, p(Pos2, _)) :-
    p_near(Middle, Positions),
    permutation(Positions, [Pos1, Pos2]).

% p_near(P, Near) is true if P has the form p/2 and Near is the list of possible
% locations for connected pipes (it depends on the shape of P).
p_near(p(I-J, '-'), [I-J1, I-J2]) :- J1 #= J-1, J2 #= J+1.
p_near(p(I-J, '|'), [I1-J, I2-J]) :- I1 #= I-1, I2 #= I+1.
p_near(p(I-J, 'F'), [I1-J, I-J1]) :- I1 #= I+1, J1 #= J+1.
p_near(p(I-J, 'L'), [I1-J, I-J1]) :- I1 #= I-1, J1 #= J+1.
p_near(p(I-J, 'J'), [I1-J, I-J1]) :- I1 #= I-1, J1 #= J-1.
p_near(p(I-J, '7'), [I1-J, I-J1]) :- I1 #= I+1, J1 #= J-1.

% Part 2 - Counting inside points.
%
% We know from elementary geometry that we can determine if a point is inside a
% loop by tracing a straight line in a random direction, and counting the number
% of intersection points: if the number is odd, we are inside, if even, outside.
%
% We choose as direction the line going left from a point (towards the beginning
% of the line), but the coarse representation imposes a closer look. In a
% horizontal line we can only observe intersections (|), or stretches of pipes
% that arrive on our line and then leave (so, they start with an angle, then a
% series of dashes, then an angle). It's easy to convince ourselves (by wiggling
% the angle of the horizontal line in our mind) that "vertical" figurations,
% like `|` and `L---7` change the inside/outside polarity, while "cup"
% figurations, like `LJ` or `F--7` don't. We can then calculate the number of
% internal points starting from the beginning of the line and using a grammar
% that keeps track of the current outside/inside polarity.

% line_inside(LineNum, N) is true if at line LineNum in the input there are N
% internal points.
line_inside(LineNum, N) :-
    side(Side),
    % For each line we construct a representation in which we have the pipe
    % symbol for locations belonging the loop, and '.' for all the others.
    aggregate_all(bag(R), (between(1, Side, J), (in_loop(p(LineNum-J, Sym)) -> R=Sym; R='.')), Line),
    % We determine the number of points inside the line
    phrase(inside(0, N), Line).

% inside(InitialParity, N) describes a line starting with parity 0 (outside),
% and containing N internal points.
%
% If a stretch of pipes is encountered, the parity changes according to pipes.
inside(P, N) --> pipes(Parity), { P1 #= (P + Parity) mod 2 }, inside(P1, N).
% There's a point, but it's external. No change in internal points.
inside(0, N) --> ['.'], inside(0, N).
% There's a point, and it's internal! Internal points increase.
inside(1, N) --> ['.'], inside(1, ξ(N-1)).
% An empty line has no internal points.
inside(_, 0) --> [].

% pipes(Parity) describes a sequence of tubing and the changes to the
% outside/inside polarity. 1 means the polarity is reversed, 0 means it's the
% same..
pipes(1) --> ['F'], list(dash), ['J'].
pipes(0) --> ['F'], list(dash), ['7'].
pipes(0) --> ['L'], list(dash), ['J'].
pipes(1) --> ['L'], list(dash), ['7'].
pipes(1) --> ['|'].
dash(_)  --> ['-'].
