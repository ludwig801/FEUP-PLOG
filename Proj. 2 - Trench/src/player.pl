% ==================================================================================
% ==================================================================================
% Predicates that handles the players
% ==================================================================================
% ==================================================================================

% Players pieces
% Returns the list of pieces for each player.
player_pieces(p1, [g1, co1, ca1, sa1, so1]).
player_pieces(p2, [g2, co2, ca2, sa2, so2]).

% checkPiecePlayer(piece, player)
% Checks the player that owns a given piece.
check_piece_player(PI, P) :- player_pieces(P, L), !, member(PI, L), !.

players([p1,p2]).

first_player(p1).

next_player(p1, p2).
next_player(p2, p1).

in_enemy_turf(p1, Row) :- Row > 8.
in_enemy_turf(p2, Row) :- Row < 8.

cpu(p1) :- fail.
%cpu(p2) :- fail.
cpu(p2).