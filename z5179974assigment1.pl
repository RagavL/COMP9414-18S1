% FULL NAME : RAGAVENDRAN LAKSHMINARASIMHAN
% STUDENT ID: z5179974
% COURSE:COMP9414 ARTIFICIAL INTELLIGENCE
% SEMESTER: SEMESTER 1,2018
% ASSIGNMENT 1 - PROLOG PROGRAMMING
%-----------------------------------------------------------------------------------------------------------------------------------------------------------------%

%-----------
% QUESTION 1|
%-----------

%This predicate is for Empty List
sumsq_neg([], 0).

%This predicate check if the Element in the list is Negative ,if It is negative adds it to the list.
sumsq_neg([Head|Tail], Sum) :-
    Head < 0,
    sumsq_neg(Tail, RestSum),
    Sum is Head*Head + RestSum.

%This predicate check if the number is positive
sumsq_neg([Head|Tail], Sum) :-
    Head >= 0,
    sumsq_neg(Tail, Sum).

%-----------------------------------------------------------------------------------------------------------------------------------------------------------------%

%-----------
% QUESTION 2|
%-----------
%The first & second predicate loops for a items in the what list with a single person
%The Third and the Fourth predicate Loops for all persons

%likes(mary, apple).
%likes(mary, pear).
%likes(mary, grapes).
%likes(tim, mango).
%likes(tim, apple).
%likes(jane, apple).
%likes(jane, mango).

all_like_all([Head|Tail], What_List) :-
	one_all_like(Head, What_List),
	all_like_all(Tail, What_List). 
all_like_all([],_). 

one_all_like(Head, [Head2|Tail2]) :-
	likes(Head, Head2),
	one_all_like(Head, Tail2).
	
one_all_like(_,[]).	


%-----------------------------------------------------------------------------------------------------------------------------------------------------------------%



%-----------
%QUESTION 3|
%-----------
% First predicate is going to kind of break the recursion and returns the elemeny & elementSQRT pair
% The second is going through the loop until N > M is true

sqrt_table(N,M,Result):-
    N=:=M,
    TSqrt is sqrt(N),
    Result = [[N,TSqrt]].

sqrt_table(N, M, Result) :-
    	N > M,
        TSqrt is sqrt(N),
        TList = [N, TSqrt],
    	TN is N - 1,
    	sqrt_table(TN,M,TResult),
        append([TList], TResult, Result).

%-----------------------------------------------------------------------------------------------------------------------------------------------------------------%


%-------------
% Question 4 |
%-------------


	



% The below functor is a helper to check whether the number and the head are sequential 
% If it is, then it is going to return true. ! 




is_it_next_successive_element([A, B | Rem], [A ,Next], Rest) :-
    A + 1 =:= B,
    is_it_next_successive_element([B | Rem], Next, Rest).

is_it_next_successive_element([A, B | Rem], A, [B | Rem]) :-
    A + 1 =\= B.

is_it_next_successive_element([Y], Y, []).


% This is the main predicate which traverse through the list and check each and every Element
% For this one it expect the is_it_the_next_number return true i.e. Head (which is the First Number) 
% and the tail (First element from this tail) are descreasing sequential. 
% The next predicate does the same thing however, it considers the false case of is_it_the_next_number.

chop_up([9,10,5,6,7,3,1], Result) :-
    Result = [[9, 10], [5, 7], 3, 1].


chop_up([1,3,2,3,4], Result) :-
    Result = [1, 3, [2, 4]].


chop_up([], []).
chop_up(List, [List1 | List2]) :-
    is_it_next_successive_element(List, List1, Rest),
    chop_up(Rest, List2).


%-----------------------------------------------------------------------------------------------------------------------------------------------------------------%

%------------
% Question 5|
%------------


% First and Second are the facts given in the question.
% Final one, traverses through the left tree and right tree and evalutate the expressions. 

tree_eval(Value, tree(empty, z, empty), Value).

tree_eval(_, tree(empty, Num, empty), Num).

tree_eval(Value, tree(LeftTree, Operator, RightTree), Eval) :-
	tree_eval(Value, LeftTree, L),
	tree_eval(Value, RightTree, R),
	Expression =.. [Operator, L, R],
	Eval is Expression.


%-----------------------------------------------------------------------------------------------------------------------------------------------------------------%
