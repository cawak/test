/** Task 1 **/

/** reverse list predicate **/
reverse([],Z,Z).
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

/**is_binary(+) function **/
zero([]).
is_binary(B) :- zero(B).
is_binary(B) :- reverse(B,X,[]), append([1],_,X), check_binary(B).
check_binary([]).
check_binary([H|T]) :- member(H, [0,1]), check_binary(T).

/** Task 2 **/

/** Here we imitate what we have learned in our introduction to computers class. We abstracted our lists as sets of bits and we
re-created boolean operations such as: xor and full adder.
**/

/** xorp(A,B,Cin,Cout,S), Cin = carry in, Cout = carry out, S = solution**/
xorp(0,0,0,0,0).
xorp(0,0,1,0,1).
xorp(0,1,0,0,1).
xorp(0,1,1,1,0).
xorp(1,0,0,0,1).
xorp(1,0,1,1,0).
xorp(1,1,0,1,0).
xorp(1,1,1,1,1).

binary_plus(X,Y,Z) :- is_binary(X), is_binary(Y), binary_plus(X,Y,0,[],Z).

/** See above. Here we use our knowlegde in boolean algebra **/
binary_plus([X|XS],[Y|YS],Cin,Acc,Z) :- xorp(X,Y,Cin,Cout,S), append(Acc,[S],Acc1), binary_plus(XS,YS,Cout,Acc1,Z).
binary_plus([],[Y|YS],Cin,Acc,Z) :- xorp(0,Y,Cin,Cout,S), append(Acc,[S],Acc1), binary_plus([],YS,Cout,Acc1,Z).
binary_plus([X|XS],[],Cin,Acc,Z) :- xorp(X,0,Cin,Cout,S), append(Acc,[S],Acc1), binary_plus(XS,[],Cout,Acc1,Z).

/** Decide whether to add last digit to number (if 1 then yes). **/
binary_plus([],[],Cin,Acc,Z) :- xorp(0,0,Cin,0,S), S=1 ,append(Acc,[1],Z).
binary_plus([],[],Cin,Acc,Z) :- xorp(0,0,Cin,0,S), S=0, append(Acc,[],Z).


/** It doesn't work with binary_plus(-,-,+) mode. For example:
    
    ?- binary_plus(X,Y,[0,1]).
    
    Here we enter an infinite loop.
    **/

    
/** binary_plus_rev predicate **/

/** we used this predicate as a loop condition **/
length_help(A,B) :- length(A,N), length(B,M), N=<M.
    

/** try to use only binary_plus **/

/** basic cases **/
binary_plus_rev(X,[],X).
binary_plus_rev([],Y,Y).
binary_plus_rev(X,Y,Z) :- binary_plus_rev_help(X,Y,Z,[],[]).


/** basically, we imitate here double looping:

for X=...
  for Y=...
  
binary_plus_rev_help is the major loop while binary_plus_rev_help1 is the minor one.
**/
binary_plus_rev_help(X,Y,Z,X1,Y1) :- length_help(X1,Z), length_help(Y1,Z), X1 \= Z, Y1 \= Z, binary_plus(X1,Y1,Z), append(X1,[],X), append(Y1,[],Y).
/** here we call another aiding predicate **/
binary_plus_rev_help(X,Y,Z,X1,Y1) :- length_help(X1,Z), length_help(Y1,Z), X1 \= Z, Y1 \= Z, binary_plus(X1,[1],X2), binary_plus_rev_help1(X,Y,Z,X2,Y1).

binary_plus_rev_help1(X,Y,Z,X1,Y1) :- length_help(X1,Z), length_help(Y1,Z), X1 \= Z, Y1 \= Z, binary_plus(Y1,[1],Y2), binary_plus_rev_help1(X,Y,Z,X1,Y2).
binary_plus_rev_help1(X,Y,Z,X1,Y1) :- length_help(X1,Z), length_help(Y1,Z), X1 \= Z, Y1 \= Z, binary_plus(X1,Y1,Z), append(X1,[],X), append(Y1,[],Y).

binary_plus_rev_help1(X,Y,Z,X1,Z) :- length_help(X1,Z), binary_plus_rev_help(X,Y,Z,X1,[]).



/** Task 3 **/

/** This is a very simple predicate after writing task 2 
We've tried to stick to this algorithm:

Acc=0;
Y1=0;
For Y1<Y do
    Acc=Acc+X1;
    Y1++;
Z=Acc;
return Z.

**/

/* basic case*/
binary_times(X,Y,Z) :- is_binary(X), is_binary(Y), binary_times_help(X,Y,Z,X,[],[]).

/** Actually, in binary_times_help(X,Y,Z,X1,Y1,Acc) X is useless. Here we check if our multiplier Y is equal to counter Y1. If that's
    true, we return Acc
**/
binary_times_help(_,Y,Acc,_,Y,Acc).
/** We add X1 (our multiplicant) to Acc if Y1 is not equal to Y **/
binary_times_help(X,Y,Z,X1,Y1,Acc) :- Y1 \= Y, binary_plus(Y1,[1],Y2) ,binary_plus(Acc,X1,Acc1), binary_times_help(X,Y,Z,X1,Y2,Acc1).


/** Task 4 **/

/* Simple arithmetics with fractions. We use 2 accumulators: nominator and denominator. If in the end they are equal then we return true. */
/* We even use gcd function but it seems the answers remained the same */

is_data_ok([],_).
is_data_ok([D|Data],N) :- N>=D, is_data_ok(Data,N).

append_by_number(X,N,Sol) :- append_by_number(X,N,[],Sol).
append_by_number([X|Xs],1, Acc, Sol) :- X1 is X+1, append(Acc,[X1],Acc1), append(Acc1,Xs, Sol).
append_by_number([X|Xs],N,Acc,Sol) :- N>1, append(Acc,[X],Acc1), N1 is N-1 ,append_by_number(Xs,N1,Acc1,Sol).

/* upper bound of division: */
div_up(N,NN) :- N1 is div(N,3), N2 is N1*3, N = N2, NN is N1.
div_up(N,NN) :- N1 is div(N,3), N2 is N1*3, N \= N2, NN is N1+1.

testFractions(0,[]).
testFractions(N,List) :- N>2, div_up(N,NN) ,testFractionshelp(N,List,[0,0,0,0,0,0,0,0,0],0,1,NN).

/* Data is a list that counts every number. For example if List is; [1,1,2,5,4,5,8,3,6,9,2,7,9,3,6] then Data is [2,2,2,1,1,2,1,1,1]. We need 
Data just to be sure our solution is correct 
*/
testFractionshelp(0,[],Data,Nom,Nom,NN) :- is_data_ok(Data,NN).

testFractionshelp(N,[X,Y,Z|List],Data,Nom,Den,NN) :- X>0, Y>0, Z>0, X<10, Y<10, Z<10, append_by_number(Data,X,Data1), append_by_number(Data1,Y,Data2),append_by_number(Data2,Z,Data3), Den1 is 10*Y+Z, Den2 is Den * Den1, Nom1 is Nom * Den1, Nom2 is X * Den, Nom3 is Nom1 + Nom2, N1 is N-1,testFractionshelp(N1, List, Data3, Nom3, Den2, NN).


/* you pick the first 6 numbers and create denominators an nominator accumulators
http://ianm.host.cs.st-andrews.ac.uk/CSPLib/prob/prob041/index.html - I think the example with 6 is wrong!
*/


/** Task 5 **/
/*
Previous implementation: it caused stack error. I leaved it here cause it seems to be correct but provokes memory issues. It scans every possible combination for correctness.

list_length(X,L) :- list_length(X,0,L).
list_length([],L,L).
list_length([_|Xs],Acc,L) :- Acc1 is Acc+1, list_length(Xs,Acc1,L).

add_to_list(L,Sol) :- add_to_list(L,[],Sol).
add_to_list([X|Xs],Acc,Sol) :- X1 is X+1, X1<10, append(Acc,[X1],X2), append(X2,Xs,Sol).
add_to_list([X|Xs],Acc,Sol) :- X1 is X+1, X1>9, append(Acc,[1],X2), add_to_list(Xs,X2,Sol).

nFractions(N,L) :- nFractionshelp(N,[1,1,1,1,1,1,1,1,1],L).

nFractionshelp(3,L,L) :- testFractions(3,L).
nFractionshelp(3,X,L) :- add_to_list(X,Sol), nFractionshelp(3,Sol,L).
*/

/*permutation: we permutate the list there's a built in function perm in prolog but we wrote our implemenataion */

choose(X,[X|Y],Y).
choose(X,[W|Y],[W|S]) :- choose(X,Y,S).
permutate([],[]).
permutate([X|Y],Z) :- permutate(Y,W), choose(X,Z,W).   

/* Knowing that N=3, our solution has to be some permutation of [1,2,3,4,5,6,7,8,9] and that's what we do: we permutate this list, untill we find the
right one. Much easier that my previous version!
*/

nFractions(N,L) :- nFractionshelp(N,[1,2,3,4,5,6,7,8,9],L).
nFractionshelp(3,X,Sol) :- permutate(X,Sol), testFractions(3,Sol).


