%------------------------------------------------------------------------------%
% War Of Life                                                                  %
%------------------------------------------------------------------------------%
% AUTHORS : Luke Shilleto, <luke.shilleto11@imperial.ac.uk>                    %
%         : Colin Stannard, <colin.stannard11@imperial.ac.uk>                  %
% DATE    : 07 March 2013                                                      %
%------------------------------------------------------------------------------%

:- ensure_loaded('war_of_life.pl').

%------------------------------------------------------------------------------%
% Strategy testing                                                             %
%------------------------------------------------------------------------------%
are_identical(X,Y):-
X==Y.

test_strategy(NumberOfGames,FirstPlayerStrategy, SecondPlayerStrategy) :-
  run_test(0, NumberOfGames, FirstPlayerStrategy, SecondPlayerStrategy,
    [], [], 0).

run_test(NumberOfGames, NumberOfGames, _, _, Winners, Moves, RunTime) :-
  include(are_identical('b'), Winners, PlayerOneWinners),
  length(PlayerOneWinners,PlayerOneWins),
  include(are_identical('r'), Winners, PlayerTwoWinners),
  length(PlayerTwoWinners,PlayerTwoWins),
  exclude(are_identical('b'), Winners, NoBlues),
  exclude(are_identical('r'), NoBlues, Draws),
  length(Draws, NumberOfDraws),
  min_member(Shortest,Moves),
  sumlist(Moves, TotalMoves),
  AverageGameLength is TotalMoves / NumberOfGames,
  delete(Moves, 250, NonExhaustiveMoves),
  max_member(Longest, NonExhaustiveMoves),
  AverageRunTime is RunTime / NumberOfGames,
  write('Number of Draws: '), write(NumberOfDraws), nl, 
  write('Number of wins for player 1 (blue): '), write(PlayerOneWins), nl, 
  write('Number of wins for player 2 (red): '), write(PlayerTwoWins), nl,
  write('Longest (non-exhaustive) game: '), write(Longest), nl,
  write('Shortest game: '), write(Shortest), nl,
  write('Average game length: '), write(AverageGameLength), nl,
  write('Average game time: '), write(AverageRunTime), nl.

   
run_test(GamesPlayed, NumberOfGames, FirstPlayerStrategy, SecondPlayerStrategy,
 Winners, Moves, RunTime) :-
  statistics(runtime, [StartTime|_]),
  play(quiet, FirstPlayerStrategy, SecondPlayerStrategy, TotalMoves, Winner),
  statistics(runtime, [EndTime|_]),
  Time is EndTime - StartTime,
  NewRunTime is RunTime + Time,
  NewGames is GamesPlayed + 1,
  run_test(NewGames, NumberOfGames, FirstPlayerStrategy, SecondPlayerStrategy, 
          [Winner|Winners], [TotalMoves|Moves], NewRunTime).




%------------------------------------------------------------------------------%
% AI Strategies                                                                %
%------------------------------------------------------------------------------%

% Find all possible moves that can be made.
possible_moves(Alive, OtherPlayerAlive, PossibleMoves) :-
  findall([A,B,MA,MB],(member([A,B], Alive),
                      neighbour_position(A,B,[MA,MB]),
	              \+member([MA,MB],Alive),
	              \+member([MA,MB],OtherPlayerAlive)),
	 PossibleMoves).

player_possible_moves(Player, [AliveBlues, AliveReds], PossibleMoves) :-
  ( Player == 'b',
    possible_moves(AliveBlues, AliveReds, PossibleMoves)
  );
  ( Player =='r',
    possible_moves(AliveReds, AliveBlues, PossibleMoves)
  ).


play_possible_moves([AliveBlues,AliveReds], PossibleMoves, Boards) :-
  % For all valid moves, apply the move to the current board and count how many
  % opponent pieces are alive after the move has been made.
  findall(
    % Produce an object containing the move made and the resulting board
    [[A,B,MA,MB], AfterCrankBoard, PlayerAliveCount, OtherPlayerAliveCount],

    % Filtering rule
    ( member([A,B,MA,MB], PossibleMoves),
      do_move([A,B,MA,MB], [AliveBlues,AliveReds], PreCrankBoard),
      next_generation(PreCrankBoard, AfterCrankBoard),

      ( member([A,B], AliveBlues),
        count_player_pieces('b', [AliveBlues,AliveReds], PlayerAliveCount),
        count_player_pieces('r', [AliveBlues,AliveReds], OtherPlayerAliveCount)
      );
      ( member([A,B], AliveReds),
        count_player_pieces('r', [AliveBlues,AliveReds], PlayerAliveCount),
        count_player_pieces('b', [AliveBlues,AliveReds], OtherPlayerAliveCount)
      )
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



other_player_count_comparison([_, _, _, Count], [_, _, Count1, _]) :-
  Count < Count1.

player_count_comparison([_, _, _, Count], [_, _, Count1, _]) :-
  Count < Count1.

landgrab_count_comparison([_, _, A, B], [_, _, A1, B1]) :-
  (A - B) < (A1 - B1).



% This strategy chooses the next move for a player to be the one which (after 
% Conway’s crank) produces the board state with the fewest number of opponent’s
% pieces on the board (ignoring the player’s own pieces).
bloodlust(Player, Board, NewBoard, Move) :-

  % Generate possible moves
  player_possible_moves(Player, Board, PossibleMoves),

  % Simulate each possible move
  play_possible_moves(Board, PossibleMoves, Boards),

  % Choose the move that results in the opponent having the fewest pieces left
  min_member(other_player_count_comparison, [Move, _, _, _], Boards),

  % Apply the chosen move to the game board
  do_move(Move, Board, NewBoard).


% This strategy chooses the next move for a player to be the one which (after
% Conway’s crank) produces the board state with the largest number of that
% player’s pieces on the board (ignoring the opponent’s pieces).
self_preservation(Player, Board, NewBoard, Move) :-

  % Generate possible moves
  player_possible_moves(Player, Board, PossibleMoves),

  % Simulate each possible move
  play_possible_moves(Board, PossibleMoves, Boards),

  % Choose the move that results in the player having the most pieces left
  max_member(player_count_comparison, [Move, _, _, _], Boards),

  % Apply the chosen move to the game board
  do_move(Move, Board, NewBoard).


% This strategy chooses the next move for a player to be the one which (after 
% Conway’s crank) produces the board state which maximises this function:
% Number of Player’s pieces – Number of Opponent’s pieces.
land_grab(Player, Board, NewBoard, Move) :-

  % Generate possible moves
  player_possible_moves(Player, Board, PossibleMoves),

  % Simulate each possible move
  play_possible_moves(Board, PossibleMoves, Boards),

  % Choose the move that results in the most land being grabbed
  max_member(landgrab_count_comparison, [Move, _, _, _], Boards),

  % Apply the chosen move to the game board
  do_move(Move, Board, NewBoard).


% This strategy looks two-ply ahead using the heuristic measure described in
% the Land Grab strategy. It should follow the minimax principle and take into
% account the opponent’s move after the one being chosen for the current player.
minimax(Player, [AliveBlues, AliveReds], NewBoard, Move) :-

  % Generate possible moves
  player_possible_moves(Player, [AliveBlues, AliveReds], PossibleMoves),

  % Simulate each possible move
  play_possible_moves([AliveBlues, AliveReds], PossibleMoves, Boards),

  % Choose the move that results in the most land being grabbed
  max_member(landgrab_count_comparison, [Move, _, _, _], Boards),

  % Apply the chosen move to the game board
  do_move(Move, [AliveBlues, AliveReds], NewBoard).



