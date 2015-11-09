% ==================================================================================================
% ==================================================================================================
%                                          ->|DRAWING|<-
% ==================================================================================================
% ==================================================================================================
% Print game state

% Parameters: List to print, Starting line
% TODO: Verify if I is bigger than the matrix rows.
print_board([], _).

print_board([H|T], I) :-
                I \= 7,
                      I2 is I * 2,
                      S is abs(14 - I2),
                      print_spaces(S),
                      print_board_left_ref(I),
                      write('  '),
                      print_board_line(H),
                      write('  '),
                      print_board_right_ref(I),
                      nl,
                      I1 is I + 1,
                      print_board(T, I1);
                I2 is 7 * 2,
                      S is abs(14 - I2),
                      print_spaces(S),
                      write('   '),
                      print_board_trench(H),
                      nl,
                      I1 is 7 + 1,
                      print_board(T, I1).

% Prints a board 
print_board([H|T]) :- print_board_header(_), print_board([H|T], 0), print_board_footer(_).
                                   

% Prints empty lines
% Parameters: S: number of line to print.
% Restrictions: S should be bigger than 0 (zero).
print_spaces(0).

print_spaces(S) :-
                S > 0,
                        write(' '),
                        S1 is S - 1,
                        print_spaces(S1).

% Prints the actual game line
print_board_line([]).

% Parameters: Game line to print (list)
print_board_line([H]) :-
                     get_board_symbol(H, S),
                     write(S).

print_board_line([H|T]) :-
                     get_board_symbol(H, S),
                     write(S),
                     write(' '),
                     print_board_line(T).

print_board_trench([]).

print_board_trench([H]) :-
                     get_board_symbol(H, S),
                     write(S).

print_board_trench([H|T]) :-
                     get_board_symbol(H, S),
                     write(S),
                     format('~c', [215]),
                     print_board_trench(T).


print_board_header(_) :-
        print_spaces(16),
        write('a'),
        print_spaces(3),
        write('i'), nl.

print_board_footer(_) :-
        print_spaces(16),
        write('p'),
        print_spaces(3),
        write('h'), nl.


% ==========================================================================
%  Print Board Structure
% ==========================================================================
print_board_left_ref(I) :- 
        I < 7, !,
                X is I + 1,
                get_symbol(X, S),
                write(S);
        I < 16, !,
                get_symbol(I, S),
                write(S).    

print_board_right_ref(I) :-
        I < 7, !,
                X is I + 9,
                get_symbol(X, S),
                write(S);
        I < 16, !,
                X is abs(8 - I),
                get_symbol(X, S),
                write(S).    


get_symbol(0, a).
get_symbol(1, b).
get_symbol(2, c).
get_symbol(3, d).
get_symbol(4, e).
get_symbol(5, f).
get_symbol(6, g).
get_symbol(7, h).

get_symbol(8, i).
get_symbol(9, j).
get_symbol(10, k).
get_symbol(11, l).
get_symbol(12, m).
get_symbol(13, n).
get_symbol(14, o).
get_symbol(15, p).

% ==========================================================================
% ==========================================================================    
             
% ==========================================================================
% Game symbols, can be changed to whatever we want 
% Returns: String containing the symbol
% ==========================================================================

% Player 1
get_board_symbol(g1, '(5)').
get_board_symbol(co1, '(4)').
get_board_symbol(ca1, '(3)').
get_board_symbol(sa1, '(2)').
get_board_symbol(so1, '(1)').

% Player 2
get_board_symbol(g2, '[5]').
get_board_symbol(co2, '[4]').
get_board_symbol(ca2, '[3]').
get_board_symbol(sa2, '[2]').
get_board_symbol(so2, '[1]').

% Empty space
get_board_symbol(e, ' - ').

% ==========================================================================
% ==========================================================================
