are_identical(X,Y):-
X==Y.

test_strategy(NumberOfGames,FirstPlayerStrategy, SecondPlayerStrategy) :-
  run_test(0, NumberOfGames, FirstPlayerStrategy, SecondPlayerStrategy, [], [], 0).


run_test(NumberOfGames, NumberOfGames, _, _,
         Winners, Moves, RunTime) :-
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

