/**

This is how the map and coordinate system works, and
also how the world is configured for this example.

north   -- move to smaller index row
south   -- move to larger index row
west   -- move to smaller index column
east  -- move to larger index column

Coordinate system:  [1,1] is at upper left, order is (row, col)
[1,1]   [1,2]  [1,3]
|-----|------|-------|
|
|  A      
|-----|------|-------|
|         P
|  WR 
|-----|------|-------|
|               
|  W       G
|------|------|------|

**/

% Initial state

:- dynamic(agent_at/1).
:- dynamic(agent_holding/1).
:- dynamic(cell_contains/2).

% Every object except the agent has a type

type(g1, gold).
type(w1, wumpus).
type(p1, pit).
type(r1, wumpus_repellent).

liftable(gold).
liftable(wumpus_repellent).

size(3).

agent_at((1,1)).
cell_contains((2,2), p1).
cell_contains((3,1), w1).
cell_contains((3,2), g1).
cell_contains((2,1), r1).

/** Look **/
look :- look_location,
				look_dead, look_stench, look_breeze, look_holding_gold, look_holding_wr, look_won,
				look_contains.

look_location :- agent_at(P), write('You are at '), 
			write(P), write('\n').

look_contains :- agent_at(P), cell_contains(P, T), 
					type(T, Type), 
					write('There is '), write(Type) , write(' here\n').
look_contains :- !.

look_holding(T) :- agent_holding(T), type(T, Type), write('You are holding '), write(Type), write('\n').
look_holding :- !.

look_holding_gold :- look_holding(g1).
look_holding_gold :- !.

look_holding_wr :- look_holding(r1).
look_holding_wr :- !.

look_dead :- dead, write('You are dead!\n'),!.
look_dead :- !.

look_won :- won, write('You have won!\n'),!.
look_won :- !.

look_stench :- agent_at(C), adjacent(C, C2), cell_contains(C2, W), type(W, wumpus), 
		write('You smell a stench!\n'), !.
look_stench :- !.

look_breeze :- agent_at(C), adjacent(C, C2), cell_contains(C2, P), type(P, pit), 
		write('You feel a breeze!\n'), !.
look_breeze :- !.

dead :- \+ agent_holding(r1),
	agent_at(C), cell_contains(C, T), type(T, wumpus), !.
dead :- agent_at(C), cell_contains(C, T), type(T, pit), !.

won :- agent_at((1,1)), agent_holding(T), type(T, gold), !.

pickup(Type) :- \+ dead, \+ won,
	agent_at(C),
	cell_contains(C,T), type(T, Type), liftable(Type),
	retract(cell_contains(C,T)),
	asserta(agent_holding(T)).

putdown(Type) :- \+ dead, \+ won,
	agent_at(C),
	agent_holding(T), type(T, Type), liftable(Type),
	asserta(cell_contains(C,T)),
	retract(agent_holding(T)).
	
move(D) :- \+ dead, \+ won,
			agent_at(C), 
			cell_toward(C, D, C2), 
			retract(agent_at(C)), 
			asserta(agent_at(C2)).

cell_toward((R, C), south, (R2, C)) :- size(S), R < S, R2 is R + 1.
cell_toward((R, C), north, (R2, C)) :- R > 1, R2 is R - 1.
cell_toward((R, C), east,  (R, C2)) :- size(S), C < S, C2 is C + 1.
cell_toward((R, C), west,  (R, C2)) :- C > 1, C2 is C - 1.

adjacent(C1, C2) :- cell_toward(C1, south, C2).
adjacent(C1, C2) :- cell_toward(C1, north, C2).
adjacent(C1, C2) :- cell_toward(C1, east, C2).
adjacent(C1, C2) :- cell_toward(C1, west, C2).



