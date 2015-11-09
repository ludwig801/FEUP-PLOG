/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

:- consult(logic).
:- consult(io).
:- consult(draw).

% play_game().
play_game :-
        write('Starting game...'), nl,
        first_player(P), write('First player: '), write(P), nl, nl, 
        initialize(L),
        print_board(L),
        play_game(L, P).
             
% play_game(GameList, Player).
play_game(L, _) :-
        game_over(L), !.
        
play_game(L, P) :-
        \+ cpu(P),
        read_player_move(L, P, NL),
        print_board(NL),
        next_player(P, NP), !,
        play_game(NL, NP).

play_game(L, P) :-
        cpu(P),
        cpu_moves(L, P, Moves),
        choose_cpu_move(L, Moves, [P1,P2], [T1,T2]),
        move_piece(L, [P1,P2], [T1,T2], NL),
        nl, write('Player 2 has decided to move from '), write([P1,P2]), write(' to '), write([T1,T2]), nl, nl,
        print_board(NL),
        next_player(P, NP), !,
        play_game(NL, NP).

% Player move
% read_player_move(GameList, Player, NewGameList)
read_player_move(L, Player, NL) :-
        write('Player: '), write(Player), nl,
        write('Select Piece (xy): '),
        readPosition([A1,A2]),
        write('Select Target (xy): '),
        readPosition([B1,B2]),
        get_piece(L, [A1,A2], P),
        check_piece_player(P, Player),
        %write('Piece selected: '), get_board_symbol(P, S), write(S), nl,
        can_move(L, [A1,A2], [B1,B2], Player), !, % Verifies if the move can be done
        move_piece(L, [A1,A2], [B1,B2], NL);
        
        %else : happens when the player inserts coordinates of a piece that it's not his
        write('> Invalid Move...'), nl,
        print_board(L),
        read_player_move(L, Player, NL).
        
% ==============================
%       Game Initialization
% ==============================
% Returns the initial game list, with all the pieces in their initial position

% g - general
% co - coronel
% ca - capitan
% sa - sargeant 
% so - soldier

game_list([[g1], [co1, co1], [ca1, ca1, ca1], [sa1, sa1, sa1, sa1], 
         [e, so1, so1, so1, e], [e, e, so1, so1, e, e], [e, e, e, so1, e, e, e], [e, e, e, e, e, e, e, e],
         [e, e, e, so2, e, e, e], [e, e, so2, so2, e, e], [e, so2, so2, so2, e], [sa2, sa2, sa2, sa2],
         [ca2, ca2, ca2], [co2, co2], [g2]]).

initialize(X) :- game_list(X).

% ==============================
%       Game Over
% ==============================
%  A game is considered over in the following situations:
%       - A players as lost all of its pieces;
%       - None of the pieces in the board has a valid move;
%       - No pieces has been 'consumed' in 30 moves.
game_over(GameList) :-
        
        
        \+ player_has_pieces(GameList, p1),
                write('Player 1 has no pieces left.'), nl,
                write('Player 2 wins'), !, nl;
        %write('player 1 has pieces'), nl,
        \+ player_has_pieces(GameList, p2),
                write('Player 2 has no pieces left.'), nl,
                write('Player 1 wins'), !, nl;
        %write('player 2 has pieces'), nl,
        
        
        \+ player_has_moves(GameList, p1, [1, 1]),
                write('Player 1 has no moves left.'), nl,
                write('Player 2 wins'), !, nl;
        %write('Player 1 has moves'), nl, %!,
        \+ player_has_moves(GameList, p2, [1, 1]),
                write('Player 2 has no moves left.'), nl,
                write('Player 1 wins'), !, nl.

% ==============================================
% checks if the player has any piece remaining
% ==============================================
% Prepares checking
player_has_pieces(GameList, Player) :-
        % Get player pieces list
        player_pieces(Player, L),
        player_has_pieces(GameList, L).

% Fail case
player_has_pieces([_|_], []) :- fail.

% For each piece checks if it exists in the gamelist. If any is found, returns yes.
player_has_pieces(GameList, [P|R]) :-
        member_matrix(P,GameList);
        player_has_pieces(GameList, R).
% ==============================================
% ==============================================

% ==============================================
% checks if the player has any pieces width a valid move
% ==============================================

% checks if the player has any pieces width a valid move
player_has_moves(GameList, Player, [X, Y]) :-
        X < 9, Y < 9,
                convert_alpha_num(R,X),
                convert_alpha_num(C,Y),
                get_piece(GameList, [R, C], P),
                check_piece_player(P, Player),
                piece_has_moves(GameList, [X, Y], [1, 1], Player).

player_has_moves(GameList, Player, [X, Y]) :-
        X < 9, !,
                X1 is X + 1,
                player_has_moves(GameList, Player, [X1, Y]);
        
        Y < 9, !,
                Y1 is Y + 1,
                player_has_moves(GameList, Player, [1, Y1]);
        fail.

% checks if a piece has a valid movement to it
piece_has_moves(GameList, [X,Y], [A1, A2], Player) :-
        A1 < 9, A2 < 9,
                convert_alpha_num(X1, X),
                convert_alpha_num(Y1, Y),
                convert_alpha_num(B1, A1),
                convert_alpha_num(B2, A2),
                %write('> CALL: '), write([X1,Y1,B1,B2]), nl,
                can_move(GameList, [X1, Y1], [B1, B2], Player).

piece_has_moves(GameList, [X,Y], [A1,A2], Player) :-
        A1 < 9, !,
                B1 is A1 + 1,
                piece_has_moves(GameList, [X,Y], [B1,A2], Player);
        A2 < 9, !,
                B2 is A2 + 1,
                piece_has_moves(GameList, [X,Y], [1,B2], Player);
        fail.


% ==============================
%       DEBUG 
% ==============================
% Debugging predicates. Not to be used in release.
print_test :- game_list(X), print_board(X).
