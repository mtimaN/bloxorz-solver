
%% Decomentați linia de mai jos pentru testare mai detaliată.
%% ATENȚIE: pe vmchecker linia este comentată.
%detailed_mode_disabled :- !, fail.
%debug_moves.

% quick ref:
% chk: verifică că este adevărat. ("check")
% uck: verifică că al doilea argument este fals. ("uncheck")
% ech: verifică că fiecare soluție pentru primul argument îndelinește
% condițiile din listă. ("each")
% nsl: verifică numărul de soluții pentru variabila din al doilea
% argument și interogarea din primul argument. ("n solutions")
% nSO: la fel ca mai sus, dar ignoră duplicatele (folosește setof)
% exp: verifică că pentru interogarea din primul argument, toate
% condițiile din listă sunt respectate.
% sSO: verifică că pentru interogarea din primul argument, soluțiile
% pentru variabila din al doilea argument sunt cele din lista dată.
% Soluțiile duplicate sunt ignorate. ("solutions set of")


tt(s0, [
       % a
       exp('empty_state(X)', [val('X')]),
       exp('empty_state(S), ttSetCell(S, 0, 0, +, _, S1)', [val('S1'), cond('S \\== S1')]),
       exp("empty_state(S), ttSetCell(S, 0, 0, 'B', _, S1), get_cell(S1, (0, 0), X)",
          ['X', tile]),
       exp("empty_state(S), ttSetCell(S, 0, 0, 'B', _, S1), get_b_pos(S1, X)",
          ['X',(0, 0)]),
       exp('ttSet0(S), get_cell(S, (0, 0), X), get_cell(S, (0, 1), Y)',
          ['X', tile, 'Y', target]),
       % f
   0.5, uck((ttSet0(S), get_cell(S, (0, 0), _)), (ttSet0(S), get_cell(S, (0, 2), _))), % celula nu există
   0.5, uck((ttSet0(S), get_cell(S, (0, 0), _)), (ttSet0(S), get_cell(S, (3, 0), _))), % celula nu există
       ech('ttSet0(S), move(S, r, S1), between(0, 2, X), get_cell(S1, (X, 0), A)',
           ['A == tile']),
       exp('ttSet0(S), get_bounds(S, Xm, XM, Ym, YM)', [
               'Xm', 0, 'XM', 2, 'Ym', 0, 'YM', 1
           ]),
       exp('ttSet0(S), move(S, r, S1), get_b_pos(S1, P)',
          [set('P', [(1, 0), (2, 0)])]),
       % k
   0.5, uck((ttSet0(S), move(S, r, _)), (ttSet0(S), move(S, d, _))), % blocul cade
   0.5, uck((ttSet0(S), move(S, r, _)), (ttSet0(S), move(S, l, _))) % blocul cade
   ]).

tt(s1, [
       % a
       exp('ttSet(1, S), get_cell(S, (0, 0), X)', ['X', tile]),
 0.25, uck((ttSet(1, S), get_cell(S, (5, 4), _)), (ttSet(1, S), get_cell(S, (9, 2), _))), % celula este goală
 0.25, uck((ttSet(1, S), get_cell(S, (5, 4), _)), (ttSet(1, S), get_cell(S, (9, 5), _))),
 0.25, uck((ttSet(1, S), get_cell(S, (5, 4), _)), (ttSet(1, S), get_cell(S, (-1, 2), _))),
 0.25, uck((ttSet(1, S), get_cell(S, (5, 4), _)), (ttSet(1, S), get_cell(S, (3, 7), _))),
       % f
       exp('ttSet(1, S), get_bounds(S, Xm, XM, Ym, YM)', [
               'Xm', 0, 'XM', 9, 'Ym', 0, 'YM', 5
           ]),
    2, exp('ttSet(1, S), ttSeq(S, [r,r,d,l,l,l,u], S1), get_b_pos(S1, P)',
           ['P', (1, 1)]),
       exp('ttSet(1, S), ttSeq(S, [d,r,r,r,r,r,r,d], S1), get_b_pos(S1, P)',
           ['P', (7, 4)]),

       % blocul cade cu o parte
       uck((ttSet(1, S), ttSeq(S, [d,r,r,r,r,r], _)), (ttSet(1, S), ttSeq(S, [d,r,r,r,r,r,r,r,r], _))),

       ech('ttSet(1, S), between(0, 5, Y), ttTiles1(L), nth0(Y, L, (A, B)),
            between(A, B, X), (X, Y) \\== (7, 4), ttSeq(S, [d,r,r,r,r,r,r,d], S1)',
           ['get_cell(S, (X, Y), T), T == tile', 'get_cell(S1, (X, Y), T), T == tile'])
   ]).

tt(s3, [
       % a
        uck((ttSet(3, S), get_cell(S, (6, 2), _)), (ttSet(3, S), get_cell(S, (0, 0), _))),
        ech("ttSet(3, S), between(0, 8, X), get_cell(S, (X, 2), T)", ['T == tile']),
        exp('ttSet(3, S), get_cell(S, (13, 3), T)', ['T', target]),
        uck((ttSet(3, S), get_cell(S, (6, 2), _)), (ttSet(3, S), get_cell(S, (5, 1), _))),
        exp("ttSet(3, S), get_bounds(S, Xm, XM, Ym, YM)",
            ['Xm', 0, 'XM', 14, 'Ym', 0, 'YM', 5]),
       % f
        exp("ttSet(3, S), ttSeq(S, [r,u,r,r,r,u], S1), get_b_pos(S1, P)",
            [set('P', [(7,0), (7,1)])]),
        chk((ttSet(3, S), ttSeq(S, [r,u,r,r,r,u,l,d,r,u,u,r,r,r,d,d,d,r,u], S1), is_final(S1))),

        % blocul cade cu totul
        uck((ttSet(3, S), ttSeq(S, [r,u,u], _)), (ttSet(3, S), ttSeq(S, [r,u,u,l,d,r,r,u,r], _))),

        % secventa de mutari nu schimba tile-urile
     2, ech("ttSet(3, S), ttSeq(S, [u,r,r,d,l,u,r,r,r,r], S1), between(1, 8, X)",
            ["get_cell(S, (X, 2), T), T == tile", "get_cell(S1, (X, 2), T), T == tile"])
    ]).

tt(s4, [
       % a
        uck((ttSet(4, S), get_cell(S, (3, 2), _)), (ttSet(4, S), get_cell(S, (0, 0), _))),
        ech("ttSet(4, S), between(2, 8, X), get_cell(S, (X, 0), T)", ['T == fragile']),
   0.3, exp('ttSet(4, S), get_cell(S, (3, 2), T)', ['T', tile]),
   0.4, exp('ttSet(4, S), get_cell(S, (11, 7), T)', ['T', fragile]),
   0.3, exp('ttSet(4, S), get_cell(S, (12, 7), T)', [val('T'), 'T', not(fragile)]),
       % f
        exp("ttSet(4, S), get_bounds(S, Xm, XM, Ym, YM)",
            ['Xm', 0, 'XM', 13, 'Ym', 0, 'YM', 8]),
        % blocul sta culcat pe celulele fragile
        exp("ttSet(4, S), ttSeq(S, [u,l,u,r,r,u], S1), get_b_pos(S1, P)",
            [set('P', [(3, 0), (3, 1)])]),
        % blocul nu sta in picioare pe celula fragila
        uck((ttSet(4, S), ttSeq(S, [u,l,u,r,r], _)),
            (ttSet(4, S), ttSeq(S, [u,l,u,r,r,u,r,r,r,r,r,r,d,r,d,d,d,l], _))),
        chk((ttSet(4, S), ttSeq(S, [u,l,u,r,r,u,r,r,r,r,r,r,d,r,d,d,d,d,d,r,u,l,l,l,l,l,l,d], S1),
            is_final(S1))),
       % j
     2, exp('ttSet(4, S), ttSeq(S, [u,l,u,r,r,u,r,r,r,r,r,r,d,r,d,d,d,d,d,r], S1), get_b_pos(S1, P)',
            ['P', (12, 7)]),
        % secventa invalida de mutari
        uck((ttSet(4, S), ttSeq(S, [u,r,d,r,r,d,r], _)))
]).

tt(s6, [
      % a
       uck((ttSet(6, S), get_cell(S, (0, 0), _))), % verifica ca celula nu exista
       ech("ttSet(6, S), between(5, 10, X), get_cell(S, (X, 0), T)", ['T == tile']),
       uck((ttSet(6, S), get_cell(S, (10, 5), _))), % verifica ca celula nu exista
       exp("ttSet(6, S), get_bounds(S, Xm, XM, Ym, YM)", % verifica limitele hartii
        ['Xm', 0, 'XM', 14, 'Ym', 0, 'YM', 9]),
       uck((ttSet(6, S), ttSeq(S, [r,r,r,d,d,l,l,u,u], S1), get_b_pos(S1, _))), % verifica secventa de mutari si pozitia finala
      % f
  1.5, chk((ttSet(6, S), ttSeq(S, [r,r,r,d,d,r,d,d,r,d,r,u,l,l,l,u,u,l,u,u,u,r,r,r,d,d,l,u,r,r,d,r,d,d,r], S1), is_final(S1))), % verifica daca ajunge la final cu secventa data
  1.5, exp('ttSet(6, S), ttSeq(S, [r,r,r,d,d,r,d,d,r,d,r,u,l,l,l,u,u,l,u,u,u,r], S1), get_b_pos(S1, P)',
            [set('P', [(6, 0), (7, 0)])]),
       uck((ttSet(6, S), ttSeq(S, [r,r,r,r,r,r,r], _))), % verifica daca blocul cade
       ech("ttSet(6, S), ttSeq(S, [r,r,r,d,d], S1), between(5, 10, X)", % verifica daca secventa nu schimba placile
        ["get_cell(S, (X, 0), T), T == tile", "get_cell(S1, (X, 0), T), T == tile"])
    ]).


tt(s2, [
       % a
       exp('ttSet(2, S), get_cell(S, (2,2), O), get_cell(S, (8,1), X)', ['O', oswitch, 'X', xswitch]),
 0.25, uck((ttSet(2, S), get_cell(S, (3,4), _)), (ttSet(2, S), get_cell(S, (4, 4), _))),
 0.25, uck((ttSet(2, S), get_cell(S, (3,4), _)), (ttSet(2, S), get_cell(S, (5, 4), _))),
 0.25, uck((ttSet(2, S), get_cell(S, (3,4), _)), (ttSet(2, S), get_cell(S, (10, 4), _))),
 0.25, uck((ttSet(2, S), get_cell(S, (3,4), _)), (ttSet(2, S), get_cell(S, (11, 4), _))),
       % f
       exp('ttSet(2, S), ttSeq(S, [u, r, r], S1), get_cell(S1, (4,4), T1), get_cell(S1, (5,4), T2)',
           ['T1', tile, 'T2', tile]),
 0.5,  uck((ttSet(2, S), ttSeq(S, [u, r, r], _)),
           (ttSet(2, S), ttSeq(S, [u, r, r], S1), get_cell(S1, (10, 4), _))),
 0.5,  uck((ttSet(2, S), ttSeq(S, [u, r, r], _)),
           (ttSet(2, S), ttSeq(S, [u, r, r], S1), get_cell(S1, (10, 5), _))),
       exp('ttSet(2, S), ttSeq(S, [u,r,r,d,r,r,u,r,r,u,d], S1),
            get_cell(S1, (10,4), T1), get_cell(S1, (11,4), T2)',
           ['T1', tile, 'T2', tile]),
       % j
       exp('ttSet(2, S), ttSeq(S, [u,r,r,d,r,r,u,r,r,u,d,d,r,r,r,r,u,l,u], S1), get_b_pos(S1, B)',
           ['B', (13, 1)]),
 0.5,  exp('ttSet(2, S), ttSeq(S, [r,d,l,u,r,u], S1), get_cell(S1, (4,4), T1), get_cell(S1, (5,4), T2)',
           ['T1', tile, 'T2', tile]),
 0.5,  exp('ttSet(2, S), ttSeq(S, [u,r,r,d,r,r,r,u,u,u], S1)',
           [cond('\\+ get_cell(S1, (10,4), _)'), cond('\\+ get_cell(S1, (11,4), T2)')]),
       exp('ttSet(2, S), ttSeq(S, [r,d,l,u,r,u,d,u], S1)',
           [cond('\\+ get_cell(S1, (4,4), _)'), cond('\\+ get_cell(S1, (5,4), T2)')]),
       exp('ttSet(2, S), ttSeq(S, [r,d,l,u,r,u,d,u,d,u], S1), get_cell(S1, (4,4), T1), get_cell(S1, (5,4), T2)',
           ['T1', tile, 'T2', tile]),
       exp('ttSet(2, S), ttSeq(S, [u,r,r,d,r,r,u,r,r,u,l,r], S1)',
           [cond('\\+ get_cell(S1, (10,4), _)'), cond('\\+ get_cell(S1, (11,4), T2)')])
   ]).

tt(s5, [
       % a
       uck((ttSet(5, S), ttSeq(S, [l,l,l,l], _)), (ttSet(5, S), ttSeq(S, [l,l,l,l,l], _))),
       chk((ttSet(5, S), ttSeq(S, [l,l,l,l,r,l,l], _))),
       exp('ttSet(5, S), ttSeq(S, [l,l,l,l,r,l,l,l,l,d,l,d,r,r,r], S1),
            get_cell(S1, (5,8), T1), get_cell(S1, (6,8), T2)',
           ['T1', tile, 'T2', tile]),
       exp('ttSet(5, S), ttSeq(S, [l,l,l,l,r,l,l,l,l,d,l,d,r,r,r,d,r,r,r], S1)',
           [cond('\\+ get_cell(S1, (5,8), _)'), cond('\\+ get_cell(S1, (6,8), T2)')]),
       exp('ttSet(5, S),
       ttSeq(S, [l,l,l,l,r,l,l,l,l,d,l,d,r,r,r,d,r,r,r,r,d,r,r,r,r,l,l,l,l,d,l,l,l,l,l,l], S1),
       get_b_pos(S1, P)',
           ['P', (1, 8)])
   ]).

tt(solve, [
       exp('ttSet(1, S), solve(S, M), ttSeq(S, M, S1), get_b_pos(S1, P)', ['P', (7, 4)]),
       exp('ttSet(1, S), solve(S, M), ttCA(S, M, P)', ['P', (7, 4)]),
       exp('ttSet(3, S), solve(S, M), ttSeq(S, M, S1), get_b_pos(S1, P)', ['P', (13, 3)]),
       exp('ttSet(3, S), solve(S, M), ttCA(S, M, P)', ['P', (13, 3)]),
       exp('ttSet(4, S), solve(S, M), ttSeq(S, M, S1), get_b_pos(S1, P)', ['P', (6, 7)]),
       exp('ttSet(4, S), solve(S, M), ttCA(S, M, P)', ['P', (6, 7)]),
       exp('ttSet(6, S), solve(S, M), ttSeq(S, M, S1), get_b_pos(S1, P)', ['P', (13, 4)]),
       exp('ttSet(6, S), solve(S, M), ttCA(S, M, P)', ['P', (13, 4)])
   ]).

tt(solveswitches, [
       exp('ttSet(2, S), solve(S, M), ttSeq(S, M, S1), get_b_pos(S1, P)', ['P', (13, 1)]),
       exp('ttSet(2, S), solve(S, M), ttCA(S, M, P)', ['P', (13, 1)]),
       exp('ttSet(5, S), solve(S, M), ttSeq(S, M, S1), get_b_pos(S1, P)', ['P', (1, 8)]),
       exp('ttSet(5, S), solve(S, M), ttCA(S, M, P)', ['P', (1, 8)])
   ]).

tt(efficiency, [
       exp('ttSet(1, S), solve(S, M), ttSeq(S, M, S1), get_b_pos(S1, P), length(M, L), !',
           ['P', (7, 4), cond('L < 20')]),
       exp('ttSet(2, S), solve(S, M), ttSeq(S, M, S1), get_b_pos(S1, P), length(M, L), !',
           ['P', (13, 1), cond('L < 100')]),
       exp('ttSet(3, S), solve(S, M), ttSeq(S, M, S1), get_b_pos(S1, P), length(M, L), !',
           ['P', (13, 3), cond('L < 40')]),
       exp('ttSet(4, S), solve(S, M), ttSeq(S, M, S1), get_b_pos(S1, P), length(M, L), !',
           ['P', (6, 7), cond('L < 75')]),
       exp('ttSet(5, S), solve(S, M), ttSeq(S, M, S1), get_b_pos(S1, P), length(M, L), !',
           ['P', (1, 8), cond('L < 140')]),
       exp('ttSet(6, S), solve(S, M), ttSeq(S, M, S1), get_b_pos(S1, P), length(M, L), !',
           ['P', (13, 4), cond('L < 60')])
   ]).

ttSet(Level, State) :- intern_load_map(Level, Map, SwitchData),
    empty_state(S0), ttSet(S0, 0, Map, SwitchData, State).
ttSet(S, _, [], _, S) :- !.
ttSet(S, Y, [Line | Lines], SD, State) :-
    ttSet(S, 0, Y, Line, SD, SL, SDO), Y1 is Y + 1,
    ttSet(SL, Y1, Lines, SDO, State).
ttSet(S, _, _, [], SDO, S, SDO) :- !.
ttSet(S, X, Y, [Cell | Line], SD, State, SDO) :-
    (   member(Cell, [o, x]), !,
        SD = [SwitchData | Rest]
    ;   SwitchData = [], Rest = SD
    ),
    ttSetCell(S, X, Y, Cell, SwitchData, ST),
    X1 is X + 1, ttSet(ST, X1, Y, Line, Rest, State, SDO).

ttSetCell(S, X, Y, Cell, _, ST) :-
    member(Cell, [' ', '.']), !,
    set_blank(S, (X, Y), ST).
ttSetCell(S, X, Y, Cell, _, ST) :-
    member(Cell, [+, -]), !,
    set_tile(S, (X, Y), ST).
ttSetCell(S, X, Y, ^, _, ST) :- !,
    set_fragile(S, (X, Y), ST).
ttSetCell(S, X, Y, Cell, [Func | Bridge], ST) :-
    member(Cell, [o, x]), !,
    cell_print(Type, Cell),
    set_switch(S, (X, Y), Type, Func, Bridge, ST).
ttSetCell(S, X, Y, 'B', _, ST) :- !,
    set_block_initial(S, (X, Y), ST).
ttSetCell(S, X, Y, $, _, ST) :- !,
    set_target(S, (X, Y), ST).




ttSeq(S, [], S) :- ( debug_moves, !, writeln("State is now:"), print_state(S) ; true).
ttSeq(S0, [M | Moves], SLast) :-
    (   debug_moves, !, writeln("State is now:"), print_state(S0), format("make move ~w~n", [M])
    ;   true),
    move(S0, M, S1),
    ttSeq(S1, Moves, SLast).

ttSeqL(Level, Moves, SLast) :- ttSet(Level, S), ttSeq(S, Moves, SLast).

ttSeqDbg(S, Moves, SLast) :- assert(debug_moves), ttSeq(S, Moves, SLast), !, retractall(debug_moves).
ttSeqDbg(_, _, _) :- retractall(debug_moves), false.

ttTiles1([(0, 2), (0, 5), (0, 8), (1, 9), (5, 9), (6, 8)]).

% cosntruieste o stare
% B++
% $++
ttSet0(S) :-
    empty_state(S0),
    ttSetCell(S0, 0, 0, 'B', _, S1),
    ttSetCell(S1, 1, 0, +, _, S2),
    ttSetCell(S2, 2, 0, +, _, S3),
    ttSetCell(S3, 0, 1, $, _, S4),
    ttSetCell(S4, 1, 1, +, _, S5),
    ttSetCell(S5, 2, 1, +, _, S).

ttCA(S, M, (XZ, YZ)) :-
    get_b_pos(S, (X, Y)),
    ttC(X:Y, up, M, Pos:XL:YL),
    XZ is XL,
    YZ is YL,
    member(Pos, [up, dn]).
ttC(A:B, P, [], P:A:B).
ttC(H1:H2, P, [M|Ms], A) :- ttC(H1:H2, P:M, OP:O1:O2), ttC(O1:O2, OP, Ms, A).
ttC(H1:H2, up:u, u:H1:H2-2). ttC(H1:H2, up:d, d:H1:H2+2). ttC(H1:H2, up:l, l:H1-2:H2). ttC(H1:H2, up:r, r:H1+2:H2).
ttC(H1:H2, dn:u, d:H1:H2-1). ttC(H1:H2, dn:d, u:H1:H2+1). ttC(H1:H2, dn:l, r:H1-1:H2). ttC(H1:H2, dn:r, l:H1+1:H2).
ttC(H1:H2, u:u, dn:H1:H2-1). ttC(H1:H2, u:d, up:H1:H2+2). ttC(H1:H2, u:l, u:H1-1:H2). ttC(H1:H2, u:r, u:H1+1:H2).
ttC(H1:H2, d:u, up:H1:H2-2). ttC(H1:H2, d:d, dn:H1:H2+1). ttC(H1:H2, d:l, d:H1-1:H2). ttC(H1:H2, d:r, d:H1+1:H2).
ttC(H1:H2, l:u, l:H1:H2-1). ttC(H1:H2, l:d, l:H1:H2+1). ttC(H1:H2, l:l, dn:H1-1:H2). ttC(H1:H2, l:r, up:H1+2:H2).
ttC(H1:H2, r:u, r:H1:H2-1). ttC(H1:H2, r:d, r:H1:H2+1). ttC(H1:H2, r:l, up:H1-2:H2). ttC(H1:H2, r:r, dn:H1+1:H2).






