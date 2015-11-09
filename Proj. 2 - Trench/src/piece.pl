% ==================================================================================
% ==================================================================================
% Description of rules for each piece of the game. 
% ==================================================================================
% ==================================================================================
% The pieces are described as follow: 
% g - general
% co - coronel
% ca - capitan
% sa - sargeant 
% so - soldier
% The number in front of the 'symbol' is the num of the player that own that piece. 

% ===============================
% ALLOWED DIRECTION
% ===============================
% Returns allowed directions for each piece.

% Player 1
get_allowed_dir_for(g1, D) :- D == northeast.
get_allowed_dir_for(g1, D) :- D == northwest.
get_allowed_dir_for(g1, D) :- D == southeast.
get_allowed_dir_for(g1, D) :- D == southwest.
get_allowed_dir_for(g1, D) :- D == north.
get_allowed_dir_for(g1, D) :- D == south.
get_allowed_dir_for(g1, D) :- D == east.
get_allowed_dir_for(g1, D) :- D == west.

get_allowed_dir_for(co1, D) :- D == northeast.
get_allowed_dir_for(co1, D) :- D == northwest.
get_allowed_dir_for(co1, D) :- D == southeast.
get_allowed_dir_for(co1, D) :- D == southwest.
get_allowed_dir_for(co1, D) :- D == north.
get_allowed_dir_for(co1, D) :- D == west.
get_allowed_dir_for(co1, D) :- D == east.

get_allowed_dir_for(ca1, D) :- D == northeast.
get_allowed_dir_for(ca1, D) :- D == northwest.
get_allowed_dir_for(ca1, D) :- D == southeast.
get_allowed_dir_for(ca1, D) :- D == southwest.
get_allowed_dir_for(ca1, D) :- D == north.
get_allowed_dir_for(ca1, D) :- D == south.

get_allowed_dir_for(sa1, D) :- D == northeast.
get_allowed_dir_for(sa1, D) :- D == northwest.
get_allowed_dir_for(sa1, D) :- D == southeast.
get_allowed_dir_for(sa1, D) :- D == southwest.
get_allowed_dir_for(sa1, D) :- D == north.

get_allowed_dir_for(so1, D) :- D == northeast.
get_allowed_dir_for(so1, D) :- D == northwest.
get_allowed_dir_for(so1, D) :- D == southeast.
get_allowed_dir_for(so1, D) :- D == southwest.

% Player 2
get_allowed_dir_for(g2, D) :- D == northeast.
get_allowed_dir_for(g2, D) :- D == northwest.
get_allowed_dir_for(g2, D) :- D == southeast.
get_allowed_dir_for(g2, D) :- D == southwest.
get_allowed_dir_for(g2, D) :- D == north.
get_allowed_dir_for(g2, D) :- D == south.
get_allowed_dir_for(g2, D) :- D == east.
get_allowed_dir_for(g2, D) :- D == west.

get_allowed_dir_for(co2, D) :- D == northeast.
get_allowed_dir_for(co2, D) :- D == northwest.
get_allowed_dir_for(co2, D) :- D == southeast.
get_allowed_dir_for(co2, D) :- D == southwest.
get_allowed_dir_for(co2, D) :- D == north.
get_allowed_dir_for(co2, D) :- D == west.
get_allowed_dir_for(co2, D) :- D == east.

get_allowed_dir_for(ca2, D) :- D == northeast.
get_allowed_dir_for(ca2, D) :- D == northwest.
get_allowed_dir_for(ca2, D) :- D == southeast.
get_allowed_dir_for(ca2, D) :- D == southwest.
get_allowed_dir_for(ca2, D) :- D == north.
get_allowed_dir_for(ca2, D) :- D == south.

get_allowed_dir_for(sa2, D) :- D == northeast.
get_allowed_dir_for(sa2, D) :- D == northwest.
get_allowed_dir_for(sa2, D) :- D == southeast.
get_allowed_dir_for(sa2, D) :- D == southwest.
get_allowed_dir_for(sa2, D) :- D == north.

get_allowed_dir_for(so2, D) :- D == northeast.
get_allowed_dir_for(so2, D) :- D == northwest.
get_allowed_dir_for(so2, D) :- D == southeast.
get_allowed_dir_for(so2, D) :- D == southwest.
% ===============================
% ===============================

% ===============================
% MAX DISTANCE
% ===============================
% Predicates that returns the maximum distance that each piece can move.
% Base case (in case an invalid piece is queried)
% Player 1
max_distance_for(g1, M) :- M is 5.
max_distance_for(co1, M) :- M is 4.
max_distance_for(ca1, M) :- M is 3.
max_distance_for(sa1, M) :- M is 2.
max_distance_for(so1, M) :- M is 1.

% Player2
max_distance_for(g2, M) :- M is 5.
max_distance_for(co2, M) :- M is 4.
max_distance_for(ca2, M) :- M is 3.
max_distance_for(sa2, M) :- M is 2.
max_distance_for(so2, M) :- M is 1.
% ===============================
% ===============================