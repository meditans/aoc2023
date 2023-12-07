:- use_module(library(dcg_tools)).
:- use_module(library(medikit)).

card(Card)     --> digit(Card) | letter(C0), {code_type(C0, to_upper(Low)), atom_codes(Card, [Low])}.
hand(Hand)     --> list(card, Hand).
line(Hand-Bid) --> hand(Hand), sp, natural(Bid), nl.
input(Lines)   --> list(line,Lines).

classify(Hand, high)        :- difs([A,B,C,D,E]), once(permutation(Hand, [A,B,C,D,E])).
classify(Hand, pair)        :- difs([A,B,C,D]),   once(permutation(Hand, [A,A,B,C,D])).
classify(Hand, double_pair) :- difs([A,B,C]),     once(permutation(Hand, [A,A,B,B,C])).
classify(Hand, three)       :- difs([A,B,C]),     once(permutation(Hand, [A,A,A,B,C])).
classify(Hand, full)        :- difs([A,B]),       once(permutation(Hand, [A,A,A,B,B])).
classify(Hand, four)        :- difs([A,B]),       once(permutation(Hand, [A,A,A,A,B])).
classify(Hand, five)        :-                    once(permutation(Hand, [A,A,A,A,A])).

compare_outcome(Cmp, V1, V2) :-
    compare_like([high, pair, double_pair, three, full, four, five], Cmp, V1, V2).

joker(j,_).
use_joker(Hand, Outcome) :-
    mapsubterms(joker, Hand, Hand1),
    aggregate_all(set(O), classify(Hand1, O), Outcomes),
    predsort(compare_outcome, Outcomes, OrderedOutcomes),
    reverse(OrderedOutcomes, [Outcome|_]).

compare_hand(Part, Comp, Hand1-_, Hand2-_) :-
    (
        Part = part1 -> DetermineValue = classify,  CardOrder = compare_like([2,3,4,5,6,7,8,9,t,j,q,k,a])
    ;   Part = part2 -> DetermineValue = use_joker, CardOrder = compare_like([j,2,3,4,5,6,7,8,9,t,q,k,a])
    ),
    maplist(DetermineValue, [Hand1, Hand2], [V1, V2]),
    compare_outcome(CompOutcome, V1, V2),
    maplist(CardOrder, CompCards, Hand1, Hand2),
    exclude(=(=), [CompOutcome | CompCards], [Comp|_]).

solution(Part, TotalWinning) :-
    phrase_from_file(input(Hands), 'input1.data'),
    predsort(compare_hand(Part), Hands, Sorted),
    enumerate(Sorted, Enumerated),
    maplist([Rank-(_-Bid), Winning]>>(Winning #= Rank*Bid), Enumerated, Winnings),
    list_sum(Winnings, TotalWinning).
