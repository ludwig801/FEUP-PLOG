% ========================
% Get Positions
% ========================

convert_to_grid_pos(R, C, Row, Col) :-
        S is R + C,
        S < 9, !,
        Row is S - 1,
        Col is C.

convert_to_grid_pos(R, C, Row, Col) :-
        S is R + C,
        Row is S - 1,
        Col is 9 - R.

convert_alpha_point([AlphaRow, AlphaCol], [NumRow, NumCol]) :-
        convert_alpha_num(AlphaRow, NumRow),
        convert_alpha_num(AlphaCol, NumCol).

% Rows
convert_alpha_num(a, 1):- !.
convert_alpha_num(b, 2):- !.
convert_alpha_num(c, 3):- !.
convert_alpha_num(d, 4):- !.
convert_alpha_num(e, 5):- !.
convert_alpha_num(f, 6):- !.
convert_alpha_num(g, 7):- !.
convert_alpha_num(h, 8):- !.

% Columns
convert_alpha_num(i, 1):- !.
convert_alpha_num(j, 2):- !.
convert_alpha_num(k, 3):- !.
convert_alpha_num(l, 4):- !.
convert_alpha_num(m, 5):- !.
convert_alpha_num(n, 6):- !.
convert_alpha_num(o, 7):- !.
convert_alpha_num(p, 8):- !.



% ========================
% ========================

% Returns the next next letter. 
% Useful since we can't make calculus with letters.
% Works in Alpha mode and like a circular list ;) 
% get_next_letter(actual, next, n/s)

get_next_letter(a, N, s) :- N = b.
get_next_letter(a, N, n) :- N = p.
get_next_letter(b, N, s) :- N = c.
get_next_letter(b, N, n) :- N = a.
get_next_letter(c, N, s) :- N = d.
get_next_letter(c, N, n) :- N = b.
get_next_letter(d, N, s) :- N = e.
get_next_letter(d, N, n) :- N = c.
get_next_letter(e, N, s) :- N = f.
get_next_letter(e, N, n) :- N = d.
get_next_letter(f, N, s) :- N = g.
get_next_letter(f, N, n) :- N = e.
get_next_letter(g, N, s) :- N = h.
get_next_letter(g, N, n) :- N = f.
get_next_letter(h, N, s) :- N = i.
get_next_letter(h, N, n) :- N = g.
get_next_letter(i, N, s) :- N = j.
get_next_letter(i, N, n) :- N = h.
get_next_letter(j, N, s) :- N = k.
get_next_letter(j, N, n) :- N = i.
get_next_letter(k, N, s) :- N = l.
get_next_letter(k, N, n) :- N = j.
get_next_letter(l, N, s) :- N = m.
get_next_letter(l, N, n) :- N = k.
get_next_letter(m, N, s) :- N = n.
get_next_letter(m, N, n) :- N = l.
get_next_letter(n, N, s) :- N = o.
get_next_letter(n, N, n) :- N = m.
get_next_letter(o, N, s) :- N = p.
get_next_letter(o, N, n) :- N = n.
get_next_letter(p, N, s) :- N = a.
get_next_letter(p, N, n) :- N = o.

        