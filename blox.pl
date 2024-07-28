:- dynamic detailed_mode_disabled/0.
:- dynamic debug_moves/0.
:- ensure_loaded('files.pl').


%% TODO
% empty_state/1
% empty_state(-SNew)
% Construiește o stare goală (fără nicio informație), care va fi dată
% primului apel set/4
empty_state(state(player((0, 0), s), [], [])).



%%%%%%
% coordonata (0, 0) este coltul din stanga/sus (chiar dacă nu există un
% tile acolo)

%% TODO
% set_tile/3
% set_tile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află o pătrățică normală.
set_tile(state(Player, T, S), H, state(Player, [(H, tile)|T], S)).



%% TODO
% set_blank/3
% set_blank(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S.
% Va fi apelat de tester doar pentru pozițiile fără pătrățele de la
% coordonate unde pentru același x și y mai mare, sau pentru același y
% și x mai mare, există pătrățele. Puteți să nu faceți nimic în acest
% predicat - depinde de cum vă reprezentați intern starea.
set_blank(S, _, S).



%% TODO
% set_target/3
% set_target(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află gaura (scopul).
set_target(state(Player, T, S), H, state(Player, [(H, target)|T], S)).



%% TODO
% set_fragile/3
% set_fragile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se o pătrățică fragilă, pe care
% blocul nu poate sta în picioare.
set_fragile(state(Player, T, S), H, state(Player, [(H, fragile)|T], S)).



%% TODO
% set_block_initial/3
% set_block_initial(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află inițial blocul, plasat în
% picioare.
set_block_initial(state(_, T, S), Pos, state(player(Pos, s), [(Pos, tile)|T], S)).



%% TODO
% get_b_pos/2
% get_b_pos(+S, -BlockPos)
% Obtine pozitia sau pozitiile blocului (în funcție de dacă blocul este
% în picioare sau culcat, ca (X, Y) sau ca [(X1, Y1), (X2, Y2)]
get_b_pos(state(player(Pos, s), _, _), Pos).
get_b_pos(state(player(Pos, Dir), _, _), [Pos, Neigh]) :- neighbor(Pos, Dir, Neigh).



%% TODO
% get_bounds/5
% get_bounds(+S, -Xmin, -Xmax, -Ymin, -Ymax).
% Obtine coordonatele limită de pe hartă la care exită celule.
get_bounds(state(_, [], _), 0, 0, 0, 0).
get_bounds(state(_, [((X, Y), _)|T], _), Xmin, Xmax, Ymin, Ymax) :-
    get_bounds(state(_, T), Xm, XM, Ym, YM),
    Xmin is min(X, Xm), Ymin is min(Y, Ym),
    Xmax is max(X, XM), Ymax is max(Y, YM).


%% TODO
% get_cell/3
% get_cell(S, Pos, Type).
% Leagă Type la tipul pătrățelei de la poziția Pos. Type trebuie legat
% la:
% tile - pentru o pătrățică obișnuită.
% fragile - pentru o pătrățică fragilă.
% target - pentru scop (gaura).
% oswitch - pentru switch de tip o.
% xswitch - pentru switch de tip x.
%
% Dacă la poziția respectivă nu se află nimic, sau este în afara
% limitelor date de get_bounds, predicatul întoarce false.
get_cell(state(_, Tiles, Switches), Pos, Type) :-
    member((Pos, Type), Tiles); member((Pos, Type, _, _, _), Switches).



%% TODO
% move/3
% move(S, Move, SNext)
% Calculează în SNext starea care rezultă din realizarea mutării Move în
% starea S.
% Mutarea este una dintre d, u, l, r.
% Întoarce false dacă mutarea duce la căderea blocului în vid (nu dacă
% blocul a ajuns la scop).
revDir(u, d). revDir(d, u). revDir(l, r). revDir(r, l).

move(state(player(Pos, s), T, S), Move, state(player(PosN, Move), NewT, NewS)) :-
    (neighbor(Pos, Move, PosN), neighbor(PosN, Move, PosN2),
    member((PosN, _), T), member((PosN2, _), T)),
    activate_oswitch(PosN, T, S, T1, S1),
    activate_oswitch(PosN2, T1, S1, NewT, NewS).

move(state(player(Pos, Dir), T, S), Move, state(player(PosN, s), NewT, NewS)) :-
    (((Dir = Move, !, neighbor2(Pos, Dir, PosN));
    (revDir(Dir, Move), !, neighbor(Pos, Move, PosN))),
    member((PosN, Tile), T), Tile \= fragile),
    activate_anyswitch(PosN, T, S, NewT, NewS).

move(state(player(Pos, Dir), T, S), Move, state(player(PosN, Dir), NewT, NewS)) :-
    (neighbor(Pos, Move, PosN), neighbor(PosN, Dir, PosN2),
    member((PosN, _), T), member((PosN2, _), T)),
    activate_oswitch(PosN, T, S, T1, S1),
    activate_oswitch(PosN2, T1, S1, NewT, NewS).



%% TODO
% is_final/1
% is_final(S)
% Întoarce adevărat dacă în starea S blocul este în picioare, pe aceeași
% poziție cu gaura (scopul).
is_final(state(player(Pos, s), Tiles, _)) :- member((Pos, target), Tiles).



%%%%%%%%%% Etapa 2

%% TODO
% set_switch/6
% set_switch(+S, +Pos, +Switch, +Func, +Positions, -SNew)
% Leagă starea SNew la o stare cu aceleași informații ca și S, și în
% plus un switch la poziția Pos, cu parametrii dați.
%
% Switch: oswitch sau xswitch.
% Func: switch, uponly sau dnonly.
% Positions: pozițiile podului.
set_switch(state(Player, T, S) , Pos, Switch, Func, Positions,
    state(Player, [(Pos, Switch)|T], [(Pos, Switch, Func, Positions, false)|S])).

/* switch characteristics:
 * Pos, Switch, Func, Positions, Pressed
 */

make_tiles([], []).
make_tiles([H|T], [(H, tile)|NewT]) :- make_tiles(T, NewT).

opposite(false, true). opposite(true, false).

% activate_switch/5
% activate_switch(+Pos, +Tiles, +Switches, -NewTiles, -NewSwitches)
activate_oswitch(Pos, T, S, NewT, NewS) :-
    ((Func = switch; Pressed = false),
    member((Pos, oswitch, Func, Positions, Pressed), S), !,
    delete(S, (Pos, oswitch, Func, Positions, Pressed), S1),
    opposite(Pressed, NPressed),
    append([(Pos, oswitch, Func, Positions, NPressed)], S1, NewS),
    change_tiles(Func, Pressed, Positions, T, NewT), !);
    (NewT = T, NewS = S).

% activate_anyswitch/5
% activate_anyswitch(+Pos, +Tiles, +Switches, -NewTiles, -NewSwitches)
activate_anyswitch(Pos, T, S, NewT, NewS) :-
    ((Func = switch; Pressed = false),
    member((Pos, Switch, Func, Positions, Pressed), S), !,
    delete(S, (Pos, Switch, Func, Positions, Pressed), S1),
    opposite(Pressed, NPressed),
    append([(Pos, Switch, Func, Positions, NPressed)], S1, NewS),
    change_tiles(Func, Pressed, Positions, T, NewT), !);
    (NewT = T, NewS = S).

change_tiles(Func, Pressed, Positions, T, NewT) :-
    make_tiles(Positions, Tiles),
    ((((Func = dnonly, Pressed = false); (Func = switch, forall(member(X, Tiles), member(X, T)))),
    findall(X, (member(X, T), \+ member(X, Tiles)), NewT));
    (((Func = uponly, Pressed = false); Func = switch),
    append(T, Tiles, NewT)));
    NewT = T.

%% TODO
% solve/2
% solve(+S, -Moves)
% Solve găsește o soluție pentru problema din starea S. Soluția este
% reprezentată ca secvența de mutări Moves.
%
% Pentru a fi soluție, mutările din Moves trebuie să ducă blocul în
% picioare pe poziția scop (target).

get_switches(state(_, _, S), S).

distance((X1, Y1), (X2, Y2), Dist) :- Dist is abs(X1 - X2 - 1) + abs(Y1 - Y2 - 1).

sort_neighs(state(player(P, _), _, _), T, Sorted) :- neighbor(P, l, L),
    neighbor(P, u, U), neighbor(P, d, D), neighbor(P, r, R),
    distance(U, T, Ud), distance(L, T, Ld), distance(R, T, Rd), distance(D, T, Dd),
    List = [(Ld, l), (Ud, u), (Dd, d), (Rd, r)],
    msort(List, Sorted).

solve(state(Player, T, S), Moves) :- member((Target, target), T), get_b_pos(state(Player, T, S), Pos),
    solve_aux([state(Player, T, S)], Moves, [Pos], Target), !.

solve_aux([state(player(Pos, s), _, _)|_], _, _, Pos) :- !.
solve_aux([S|Q], [Move|Moves], Vis, Target) :- sort_neighs(S, Target, Smoves),
    member((_, Move), Smoves), move(S, Move, S1), get_switches(S, Sw), get_switches(S1, Sw1), get_b_pos(S1, Pos),
    (\+ member(Pos, Vis), (Pos \= [_]; reverse(Pos, Rev), \+ member(Rev, Vis)); Sw \= Sw1), append(Q, S1, Q1),
    solve_aux(Q1, Moves, [Pos|Vis], Target).
