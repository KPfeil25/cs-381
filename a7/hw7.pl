/* course(course_number, course_name, credits) */

course(cs101,python, 2).
course(mth210, calculusI, 5).
course(cs120, web_design, 3).
course(cs200, data_structures, 4).
course(cs210, algorithms, 4).
course(wrt101, basic_writing, 3).

/* section(CRN, course_number) */

section(1522,cs101).
section(1122,cs101).
section(2322,mth210).
section(2421,cs120).
section(8522,mth210).
section(1621,cs200).
section(7822,mth210).
section(2822,cs210).
section(3522,wrt101).

/* place( CRN, building, time) */

place(1522,owen102,10).
place(1122,dear118,11).
place(2322,with210,11).
place(2421,cov216,15).
place(8522,kec1001,13).
place(1621,cov216,14).
place(7822,kec1001,14).
place(2822,owen102,13).
place(3522,with210,15).

/* enroll(sid, CRN) */

enroll(122,1522).
enroll(122,8522).
enroll(150,1522).
enroll(150,2421).
enroll(212,7822).
enroll(300,2822).
enroll(300,8522).
enroll(310,3522).
enroll(310,8522).
enroll(310,1621).
enroll(175,2822).
enroll(175,7822).
enroll(175,3522).
enroll(410,1621).
enroll(410,7822).
enroll(113,3522).

/* student(sid, student_name, major) */

student(122, mary, cs).
student(150, john, math).
student(212, jim, ece).
student(300, lee, cs).
student(310, pat, cs).
student(175, amy, math).
student(410, john, cs).
student(113, zoe, ece).

% ---------- Question 1 ----------

% Part a
schedule(A,B,C,D) :- enroll(A,X), section(X,Y), place(X,C,D), course(Y,B,_).

% Part b
schedule(A,B,C) :- student(A,B,_), enroll(A,Y), section(Y,Z), course(Z,C,_).

% Part c
offer(A,B,C,D) :- course(A,B,_), section(C,A), place(C,_,D).

% Part d
conflict(A,B,C) :- enroll(A,B), place(B, B1, T1), enroll(A,C), place(C, B2, T2), B1 \= B2, T1 == T2.

% Part e
meet(A,B) :- enroll(A,X), enroll(B,Y), X == Y, A \= B.
meet(A,B) :- enroll(A, C1), enroll(B, C2), place(C1, B1, T1), place(C2, B2, T2), B1 == B2, (T1 =:= (T2 - 1) ; T2 =:= (T1 - 1)).

% Part f
roster(A,B) :- enroll(X,A), student(X,B,_).

% Part g
highCredits(A) :- course(_,A,Y), (Y >= 4 -> course(_,A,_) ; false).

% ---------- Question 2 ----------

% Part a
% Define helpers to check if the element is in the list or not
inList(A,[A|_]).
notinList(A,[B|_]) :- A \= B.
notinList(_,[]).

rdup([],[]).
rdup([A|B], Z) :- inList(A,B), rdup(B,Z), !.
rdup([A|Y], [A|Z]) :- notinList(A,Y), rdup(Y,Z), !.

% Part b
flat([],[]) :- !.
flat([A], Z) :- is_list(A), flat(A,Z), !.
flat([X],[X]) :- !.
flat([X|Y],M) :- flat([X], [Z]), flat(Y,W), is_list(Z), append(Z,W,M), !.
flat([X|Y],[Z|W]) :- flat([X], [Z]), flat(Y,W), !.

% Part c
dec([], []) :- !.
dec([A],[Z]) :- Z is A - 1, !.
dec([X|Y],[W|Z]) :- W is X - 1, dec(Y,Z), !.

project(_,[],[]) :- !.
project([],_,[]) :- !.
project([1|Y],[A|B],[A|Z]) :- dec(Y,W), project(W,B,Z), !.
project(X,[_|B],Z) :- dec(X,W), project(W,B,Z), !.