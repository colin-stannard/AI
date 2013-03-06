
% Find all possible moves that can be made.
possible_moves(Alive, OtherPlayerAlive, PossibleMoves) :-
  findall([A,B,MA,MB],(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
	              \+member([MA,MB],Alive),
	              \+member([MA,MB],OtherPlayerAlive)),
	 PossibleMoves).


play_possible_moves(InitialBoard, PossibleMoves, Boards) :-
  % For all valid moves, apply the move to the current board and count how many
  % opponent pieces are alive after the move has been made.
  findall(
    % Produce an object containing the move made and the resulting board
    [CurrentMove, AfterCrankBoard],

    % Filtering rule
    ( member(CurrentMove, PossibleMoves),
      do_move(CurrentMove, InitialBoard, PreCrankBoard),
      next_generation(PreCrankBoard, AfterCrankBoard)
    ),

    % List of generated objects
    Boards
  ).

do_move([A,B,MA,MB], [AliveBlues, AliveReds], [NewAliveBlues, NewAliveReds]) :-
% Blue player makes a move
( member([A,B], AliveBlues),
  alter_board([A,B,MA,MB], AliveBlues, NewAliveBlues),
  NewAliveReds = AliveReds,
  !
);
% Red player makes a move
( member([A,B], AliveReds),
  alter_board([A,B,MA,MB], AliveReds, NewAliveReds),
  NewAliveBlues = AliveBlues,
  !
).



count_player_pieces('b', [ActiveBlues, _], Count) :-
  length(ActiveBlues, Count).

count_player_pieces('r', [_, ActiveReds], Count) :-
  length(ActiveReds, Count).


opponent('b', 'r').
opponent('r', 'b').

redmin([_, Board], [_, Board1]) :-
  count_player_pieces('r', Board, Count),
  count_player_pieces('r', Board1, Count1),
  Count < Count1.

bluemin([_, Board], [_, Board1]) :-
  count_player_pieces('b', Board, Count),
  count_player_pieces('b', Board1, Count1),
  Count < Count1.


% This strategy chooses the next move for a player to be the one which (after 
% Conway’s crank) produces the board state with the fewest number of opponent’s
% pieces on the board (ignoring the player’s own pieces).
bloodlust(Player, [AliveBlues, AliveReds], NewBoard, Move) :-

  % Generate possible moves
  ( Player == 'b',
    possible_moves(AliveBlues, AliveReds, PossibleMoves)
  );
  ( Player =='r',
    possible_moves(AliveReds, AliveBlues, PossibleMoves)
  ),

  % Simulate each possible move
  play_possible_moves([AliveBlues, AliveReds], PossibleMoves, Boards),

  ( Player == 'b',
    min_member(bluemin, [Move, NewBoard], Boards)
  );
  ( Player =='r',
    min_member(redmin, [Move, NewBoard], Boards)
  ).


