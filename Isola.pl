/*--------------------------------------------------------------------------
CCP2410: Logic Programming
Practical Title: Implementation of a two player game in Prolog
Game: Isola
Authors: Petros Kefalas
Editor: Petros Grammatikopoulos
--------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------
Initial Board (all positions empty) except the ones with c and h which are
the two players (computer and human)
--------------------------------------------------------------------------*/
board([

	(1,1,-), (2,1,-),(3,1,-),(4,1,c),(5,1,-),(6,1,-),(7,1,-),
	(1,2,-), (2,2,-),(3,2,-),(4,2,-),(5,2,-),(6,2,-),(7,2,-),
	(1,3,-), (2,3,-),(3,3,-),(4,3,-),(5,3,-),(6,3,-),(7,3,-),
	(1,4,-), (2,4,-),(3,4,-),(4,4,-),(5,4,-),(6,4,-),(7,4,-),
	(1,5,-), (2,5,-),(3,5,-),(4,5,-),(5,5,-),(6,5,-),(7,5,-),
	(1,6,-), (2,6,-),(3,6,-),(4,6,-),(5,6,-),(6,6,-),(7,6,-),
	(1,7,-), (2,7,-),(3,7,-),(4,7,h),(5,7,-),(6,7,-),(7,7,-)
     ]).

/*--------------------------------------------------------------------------
Player names. C is computer. Only used in gameInit/2 for ease and abstraction.
--------------------------------------------------------------------------*/
player([h,c]).

/*--------------------------------------------------------------------------
game/0: The main predicate that starts the program. 
--------------------------------------------------------------------------*/

game:-
	player(Players),
	board(Board),
	gameInit(Players,Board),!.


/*--------------------------------------------------------------------------
gameInit/2: Makes the first move of the game.
- r means random player, so we pick a random player from the players list
- h means human
- c means computer
There could be more human or computer players, even with different names, depending on the game implementation.
--------------------------------------------------------------------------*/
gameInit(Players,Board):-
	choice(X),
	(isRandom(X)->
		random_member(Y, Players),
		play(Y,Board)
		;
		play(X,Board)
	).

/*--------------------------------------------------------------------------
choice/1: Prompt for first move choice. Used in gameInit/2 for abstraction.
--------------------------------------------------------------------------*/

choice(X):-
	writelist(['Please select who plays first (h for human, c for computer, r for random)',nl]),% We cannot have a player named r
	read(X).

/*--------------------------------------------------------------------------
isRandom/1: Check to make gameInit/2 more readable.
--------------------------------------------------------------------------*/

isRandom(r).


% ----------- MODIFY TO ANSWER Q4


/*--------------------------------------------------------------------------
play/2: +Player, +Board
a recursive deinition that implements the two moves of the each player;
if a player cannot moves, the base case ends the game.
--------------------------------------------------------------------------*/

play(Player,Board):-
	pp(Board),
	move_to(Player,Board, InterimBoard),!,
	cross_out(Player, InterimBoard, NewBoard),
	switch(Player,OtherPlayer),
	play(OtherPlayer,NewBoard).
play(_,_):-
	writelist(['END OF ISOLA GAME!!',nl]),!.

/*--------------------------------------------------------------------------
move_to/3: +Player, +Board, -NewBoard
move_to finds the available options and selects one of them.
--------------------------------------------------------------------------*/

move_to(Player,Board, NewBoard):-
	member((X,Y,Player),Board),
	find_options((X,Y), Board, Options),
	writelist([Player, ' has the Options to move ',Options,nl]),
	select_a_move((X,Y,Player), Board, Options, NewBoard).

/*--------------------------------------------------------------------------
select_a_move/4:+Position, +Board, +Options, -NewBoard
selctes one move from available options and makes the rearrangments in board 
--------------------------------------------------------------------------*/

select_a_move((_,_,Player), _, [], _):-
		switch(Player,OtherPlayer),
		writelist([Player, ' cannot move anymore! ISOLA!!',nl,OtherPlayer,' WINS!!',nl,nl,nl]),
		!,
		fail.

select_a_move((X,Y,Player), Board, Options, NewBoard):-
		select_one_to_move(Player, Options, (NX,NY)),
		replace((X,Y,-),Board,TempBoard),!,
		replace((NX,NY,Player),TempBoard,NewBoard),
		writelist([Player,' selects ', (NX,NY), ' to move.',nl]),!.
	
/*--------------------------------------------------------------------------
find_options/3: +Coordinate, +Board, -Options
finds all next available positions
--------------------------------------------------------------------------*/


find_options((X,Y), Board, Options):-
	findall((NX,NY),(next(X,Y,NX,NY),member((NX,NY,-),Board)),Options).


% ----------- ANSWERS TO Q5


/*--------------------------------------------------------------------------
next/4: +X, +Y, -NewX, -NewY
the adjacent squares of a position (X,Y)
--------------------------------------------------------------------------*/

next(X,Y,NX,Y):- X=<6, NX is X+1.
next(X,Y,X,NY):- Y=<6, NY is Y+1.
next(X,Y,NX,Y):- X>1, NX is X-1.
next(X,Y,X,NY):- Y>1, NY is Y-1.
next(X,Y,NX,NY):- NX is X-1,next(X,Y,X,NY).
next(X,Y,NX,NY):- NX is X+1,next(X,Y,X,NY).

% ----------- ANSWERS TO Q3


/*--------------------------------------------------------------------------
replace/3: +What, +WhereList, -NeList
replaces the item in the position 
--------------------------------------------------------------------------*/

replace((X,Y,N),Board,NewB):-
	select((X,Y,_),Board,(X,Y,N),NewB),!.
	

% ----------- ANSWERS TO Q2

/*--------------------------------------------------------------------------
crossout/3: +Player, +Board, -NewBoard
selects an position among the available in order to mark it as crossed
--------------------------------------------------------------------------*/

cross_out(Player,Board, NewBoard):-
	select_a_position_to_cross(Player, Board,(X,Y)),!,
	replace((X,Y,x),Board,NewBoard),!,
	writelist([Player,' selects ', (X,Y), ' to cross out.',nl]),!.


/*--------------------------------------------------------------------------
select_one_to_move/3: +Player (c or h), +Options, -Selection
from a list of options it chooses one:
- for c this happens randomly
- for h this is from user input
--------------------------------------------------------------------------*/

select_one_to_move(c,List,(X,Y)):-
	repeat,
	(random_member((X,Y),List)->
		!;
		fail
	).
	



% ----------- ANSWERS TO Q6

select_one_to_move(h,List,(X,Y)):-
	repeat,
	writelist(['Please select a square (X,Y) to move from the empty squares:',nl]),
	read((X,Y)),
	(member((X,Y),List)->
		!;
		(
			writelist([(X,Y),' is not available. Try again!',nl]),
			fail
		)
	).
	

/*--------------------------------------------------------------------------
select_a_position_to_cross/3: +Player (c or h), +Board, -Coordinate
from a list of options it selects a square to cross
- for c this happens randomly
- for h this is from user input
--------------------------------------------------------------------------*/

select_a_position_to_cross(c, Board,(X,Y)):-
	repeat,
	(random_member((X,Y,-),Board)->
		!;
		fail
	).
	

select_a_position_to_cross(h, Board,(X,Y)):-
	repeat,
	writelist(['Please select a square (X,Y) to cross:',nl]),
	read((X,Y)),
	(member((X,Y,-),Board)->
		!;
		(
			writelist([(X,Y),' is not available. Try again!',nl]),
			fail
		)
	).



% ----------- ANSWERS TO Q7

/*--------------------------------------------------------------------------
switch/2: +Player, -TheOtherPlayer
swicthes between players so that each one plays in turn 
--------------------------------------------------------------------------*/

switch(c,h).
switch(h,c).

/*--------------------------------------------------------------------------
pp/1:+Board
it prints out the board after each move
--------------------------------------------------------------------------*/

pp(Board):-
	cls,
	nl,write('--|---+---+---+---+---+---+---|'),
	member(Y,[7,6,5,4,3,2,1]),
	nl, write(Y), write(' |'),
	member(X,[1,2,3,4,5,6,7]),
	member((X,Y,C),Board),
	display_square(X,Y,C),
	fail.
pp(_):-
	!,
	nl,write('  | 1 | 2 | 3 | 4 | 5 | 6 | 7 |'),!,nl,
	nl,!.


% To diplay -- as empty squares while all others normally


display_square(7,_,-):-!,
	writelist(['   |']),!,
	nl,write('--|---+---+---+---+---+---+---|'),!.
display_square(7,_,C):-!,
	writelist([' ',C,' |']),!,
	nl,write('--|---+---+---+---+---+---+---|'),!.
display_square(X,_,-):-!,
	X\=7,
	writelist(['   |']),!.
display_square(X,_,C):-!,
	X\=7,
	writelist([' ',C,' |']),!.


/*--------------------------------------------------------------------------
in_position/3: +Nth, +List, -Selected Item
From a list of items selects the one in the Nth position
--------------------------------------------------------------------------*/

in_position(1,[H|_],H):-!.
in_position(N,[_|T],X):-
	N > 1,
	N1 is N-1,
	in_position(N1,T,X).
	
/*------------------------------------------------------------------------
writelist/1: +List
Prints all the elements of the input list
------------------------------------------------------------------------*/
writelist([]):-!.
writelist([nl|R]):-
	nl,!,
	writelist(R).
writelist([H|T]):-
	write(H),!,
	writelist(T).

/*------------------------------------------------------------------------
cls/0: clears the screen
------------------------------------------------------------------------*/
cls :-  put(27), put("["), put("2"), put("J").