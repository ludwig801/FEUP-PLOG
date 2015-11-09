% ===========================================
%       "Includes" <-- haha C rocks
% ===========================================
:- consult(list_stuff).
:- consult(converter).
:- consult(piece).
:- consult(player).

% ===========================================
%       Pieces getter & setter
% ===========================================
% Predicate that effectively moves a piece from one place to another.
% Here all movements and verifications are made. 
% movePiece(GameList, [From], [To], NewGameList).
%       Coordinates in alpha
move_piece(L, [P1, P2], [T1, T2], NL) :-
        get_piece(L, [P1, P2], P),
        get_direction([P1, P2], [T1, T2], D),
        %write('move_piece -> capture_all()'), nl,
        R = P1, C = P2,
        capture_all(L, [P1, P2], [R, C], [T1, T2], D, L1),
        set_piece(L1, P, [T1, T2], NL).

capture_all(L, [P1, P2], [R,C], [T1, T2], D, NL) :-
        
        R == T1, C == T2, NL = L;
        
        set_piece(L, e, [R,C], L1),
        inc([R,C], [TR,TC], D),
        capture_all(L1, [P1, P2], [TR, TC], [T1, T2], D, NL).
        
% ===========================================
%       Pieces getter & setter
% ===========================================
% getPiece(GameList, [Row, Column], Piece).
%       Coordinates in alpha
get_piece(L, [R, C], P) :-
        convert_alpha_point([R, C], [R1, C1]),  
        convert_to_grid_pos(R1, C1, Row, Col),
        select_elem(L, Row, Col, P).

% setPiece(GameList, Piece, [Pos], NewGameList)
%       Coordinates in alpha
set_piece(L, P, [R, C], NL) :-
        convert_alpha_point([R, C], [R1, C1]),  
        convert_to_grid_pos(R1, C1, Row, Col),
        nth1(Row, L, X),
        replace(X, Col, P, Res),
        replace(L, Row, Res, NL).

% ===========================================
%       Pieces movement
% ===========================================
% Predicate that checks if a given piece can move from a pos to another.

% canMove(GameList, [FromX,FromY], [ToX,ToY], Player)
%       Coordinates in alpha
can_move(L, [R1, C1], [R2, C2], Player) :-
        R1 == R2, C1 == C2,
        %write('>>> fail: '), write([R1,C1,R2,C2]), nl,
        !, fail;
        
        % else
        get_piece(L, [R1, C1], PI),
        %write('Piece: '), write(PI), nl,
        get_distance([R1, C1], [R2, C2], DIST),
        %write('Distance: '), write(DIST), nl,
        max_distance_for(PI, MAX),
        %write('Max: '), write(MAX), nl,
        DIST < (MAX + 1),       % Distance verification
        get_direction([R1, C1], [R2, C2], DIR),
        %write('Direction: '), write(DIR), nl,
        get_allowed_dir_for(PI, DIR),
        %write('Ei men passou tudo'), nl,
        !,
        check_road(L, [R1,C1], [R1,C1], [R2,C2], Player, 0).


% ===========================================
% DISTANCE
% ===========================================
% getDistance([SrcRow, SrcCol], [DestRow, DestCol], distance)
% Checks first if the movement is diagonal or perpendicular. 
% If only one of the coordinates changes: diagonal
% If both coordinates changes: perpendicular
% We use a generic method for calculating the distance, passing to it the two coordinates that will 
%   make the distance.
get_distance([R1, C1], [R2, C2], D) :- 
        R1 == R2, C1 \== C2, convert_alpha_point([C1, C2], [X, Y]),  calculate_distance(X, Y, D).

get_distance([R1, C1], [R2, C2], D) :-
        R1 \== R2, C1 == C2, convert_alpha_point([R1, R2], [X, Y]), calculate_distance(X, Y, D).

get_distance([R1, C1], [R2, C2], D) :-
        R1 \== R2, C1 \== C2,convert_alpha_point([R1, R2], [X, Y]), calculate_distance(X, Y, D).

% Calculates the distance between two coordinates.
calculate_distance(SRC, DST, D) :-
        SRC \== DST, D is abs(SRC - DST).
% ===========================================
% ===========================================

% ===========================================
% DIRECTION
% ===========================================
% get_direction([SrcX, SrcY], [DestX, DestY], Direction)) : Coordinates in alpha mode
get_direction([R1, C1], [R2, C2], D) :-
        
        R1 == R2, C1 \= C2,
        convert_alpha_point([C1, C2], [Y1, Y2]),
        Y2 > Y1, D = southeast;

        R1 == R2, C1 \= C2,
        convert_alpha_point([C1, C2], [Y1, Y2]),
        Y2 < Y1, D = northwest;
        
        C1 == C2,  R1 \= R2,
        convert_alpha_point([R1, R2], [X1, X2]),
        X2 > X1, D = southwest;

        C1 == C2, R1 \= R2,
        convert_alpha_point([R1, R2], [X1, X2]),
        X2 < X1, D = northeast;
        
        R1 \= R2, C1 \= C2,
        convert_alpha_point([R1, C1], [X1, Y1]),
        convert_alpha_point([R2, C2], [X2, Y2]),
        IncX is X2 - X1, IncY is Y2 - Y1, 
        IncX == IncY, X2 > X1, D = south;
        
        R1 \= R2, C1 \= C2,
        convert_alpha_point([R1, C1], [X1, Y1]),
        convert_alpha_point([R2, C2], [X2, Y2]),
        IncX is X2 - X1, IncY is Y2 - Y1, 
        IncX == IncY, X2 < X1, D = north;
        
        R1 \= R2, C1 \= C2,
        convert_alpha_point([R1, C1], [X1, Y1]),
        convert_alpha_point([R2, C2], [X2, Y2]),
        IncX is X1 - X2, IncY is Y2 - Y1, 
        IncX == IncY, X1 > X2, D = east;

        R1 \= R2, C1 \= C2,
        convert_alpha_point([R1, C1], [X1, Y1]),
        convert_alpha_point([R2, C2], [X2, Y2]),
        IncX is X1 - X2, IncY is Y2 - Y1, 
        IncX == IncY, X1 < X2, D = west.
        
   

% ===========================================
% VERIFICATION OF TRAJECTS
% ===========================================
%       Coordinates in alpha
check_road(L, [P1,P2], [R1,C1], [T1, T2], Player, Count) :-
        
        % Finish it
        R1 == T1, C1 == T2, !,
                %write('--> can_capture()'), nl,
                can_capture(L, [P1,P2], [T1,T2], Player);

        get_direction([P1, P2], [T1, T2], D),
        is_valid_house(L, [P1,P2], [R1,C1], Player, Count),
        %write('Direction: '), write(D), nl,
        inc([R1,C1], [TR, TC], D),
        Count1 is Count + 1, !,
        check_road(L, [P1,P2], [TR, TC], [T1, T2], Player, Count1);
        
        !, fail.
      
% ===========================================
%   Checking stuff
% ===========================================
is_valid_house([_|_], [_,_], [_,_], _, 0) :- !.
        
is_valid_house(L, [P1,P2], [R,C], Player, Count) :-
        Count > 0,
                is_empty_piece(L, [R,C]);
        
        Count > 0,
                is_trench([P1,P2]),
                get_piece(L, [R, C], Piece),
                \+ check_piece_player(Piece, Player).

can_capture(L, [P1,P2], [T1,T2], Player) :-
        
        is_empty_piece(L, [T1,T2]);
        %write('it\'s soooo true'), nl;
        
        is_trench([P1,P2]), is_trench([T1,T2]), !, fail;
        
        is_trench([T1,T2]),
                get_piece(L, [T1, T2], Piece),
                \+ check_piece_player(Piece, Player),
                convert_alpha_point([P1, P2], [X, Y]),
                convert_to_grid_pos(X, Y, Row, _),
                %write('Row: '), write(Row), nl,
                in_enemy_turf(Player, Row);
        
        get_piece(L, [T1, T2], Piece),
        \+ check_piece_player(Piece, Player).

%
is_empty_piece(L, [R,C]) :-
        get_piece(L, [R,C], e).

% ===========================================
% Le Trench
% ===========================================
is_trench([R, C]) :-
        convert_alpha_point([R, C], [X, Y]),
        convert_to_grid_pos(X, Y, Row, _),
        Row == 8.

% ===========================================
%  lel
% ===========================================
inc([R,C], [NextR, NextC], D) :-
        
        % southeast
        D == southeast,
                get_next_letter(C, NextC, s),
                NextR = R;
                %write(NextR), nl;
        
        % northwest
        D == northwest, 
                get_next_letter(C, NextC, n),
                NextR = R;
                %write([NextR, NextC]), nl;
        
        % southwest
        D == southwest,
                get_next_letter(R, NextR, s),
                NextC = C;
                %write([NextR, NextC]), nl;
        
        % northeast
        D == northeast, 
                get_next_letter(R, NextR, n),
                NextC = C;
                %write([NextR, NextC]), nl;
        
        % south
        D == south, 
                get_next_letter(R, NextR, s),
                get_next_letter(C, NextC, s);
        
        % north
        D == north,
                get_next_letter(R, NextR, n),
                get_next_letter(C, NextC, n);
        
        % east
        D == east,
                get_next_letter(R, NextR, n),
                get_next_letter(C, NextC, s);
        
        % west
        D == west, 
                get_next_letter(R, NextR, s),
                get_next_letter(C, NextC, n).
                

% ===========================================
%       CPU LOGIC
% ===========================================

:- use_module(library(random)).

/*
test :- game_list(X), cpu_moves(X, p2, [1,1], Moves),
        write('Moves: '), write(Moves), nl, nl,
        choose_cpu_move(X, Moves, [P1,P2], [T1,T2]),
        write('   Pos: '), write([P1,P2]), nl,
        write('Target: '), write([T1,T2]), nl.

test2 :- game_list(X), write(X).
*/

cpu_moves(GameList, Player, Moves) :-
        cpu_moves(GameList, Player, [1, 1], Moves).

cpu_moves(GameList, Player, [X, Y], Moves) :-
        Y == 9, Moves = [];

        X < 9,
                convert_alpha_num(R,X),
                convert_alpha_num(C,Y),
                get_piece(GameList, [R, C], P),
                check_piece_player(P, Player), !,
                cpu_piece_has_moves(GameList, [X, Y], [1, 1], Player, PieceMoves), !,
                X1 is X + 1,
                cpu_moves(GameList, Player, [X1, Y], NewMoves),
                append([[R,C]],PieceMoves, NewPieceMoves),
                %write(NewPieceMoves), nl,
                append([NewPieceMoves], NewMoves, Moves);
        
        X < 9, !,
                X1 is X + 1,
                cpu_moves(GameList, Player, [X1, Y], Moves);
        
        Y < 9, !,
                Y1 is Y + 1,
                cpu_moves(GameList, Player, [1, Y1], Moves).

%
cpu_piece_has_moves(GameList, [R,C], [TR,TC], Player, PieceMoves) :-
        
        TC == 9, PieceMoves = [];
        
        TR < 9,
                NR is TR + 1,
                %write('>> call : '), write([R,C]), write([TR,TC]), nl,
                %write('New: '), write(NewPieceMoves), nl,
                %write('>>> try move: '), write([A1, A2]), write([B1, B2]), nl,
                convert_alpha_point([A1,A2], [R,C]),
                convert_alpha_point([B1,B2], [TR,TC]),
                can_move(GameList, [A1, A2], [B1, B2], Player), !,
                %write('>>> can move'), nl,
                cpu_piece_has_moves(GameList, [R,C], [NR,TC], Player, NewPieceMoves), !,
                append([[B1,B2]], NewPieceMoves, PieceMoves);
                %write(PieceMoves), nl;
        
        % can_move failed
        TR < 9, !,
                NR is TR + 1,
                cpu_piece_has_moves(GameList, [R,C], [NR,TC], Player, NewPieceMoves),
                append([], NewPieceMoves, PieceMoves);
                %write(PieceMoves), nl;
        
        TC < 9, !,
                NC is TC + 1,
                cpu_piece_has_moves(GameList, [R,C], [1,NC], Player, PieceMoves).

choose_cpu_move(_, Moves, [P1,P2], [T1,T2]) :-
        length(Moves, ML),
        random(1, ML, RandomRow),
        select_row(Moves, RandomRow, Row),
        select_col(Row, 1, [P1,P2]),
        length(Row, RL),
        random(2, RL, RandomCol),
        select_col(Row, RandomCol, [T1,T2]).