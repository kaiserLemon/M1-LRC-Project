get_type(T, Res) :- term_to_atom(T, A), atom_chars(A, ABis), getSS(ABis, SS), atom_chars(Res, SS).

getSS([], []).
getSS(['(' | _], [])      :- !.
getSS([C | L], [C | Res]) :- getSS(L, Res).

triAbox(Abi, Lie, Lpt, Li, Lu, Ls)  :- getLie(Abi, [], Lie)
                                    , getLpt(Abi, [], Lpt)
                                    , getLi(Abi, [], Li)
                                    , getLu(Abi, [], Lu)
                                    , getLs(Abi, [], Ls, Lie, Lpt, Li, Lu).

getLie([], L, Lie)                              :- Lie = L.
getLie([(I, some(R, C)) | L], M, Lie)           :- concat(M, [(I, some(R, C))], Y), getLie(L, Y, Lie).
getLie([(_, _) | L], M, Lie)                    :- getLie(L, M, Lie).

getLpt([], M, Lpt)                              :- Lpt = M.
getLpt([(I, all(R, C)) | L], M, Lpt)            :- concat(M, [(I, all(R, C))], Y), getLpt(L, Y, Lpt).
getLpt([(_, _) | L], M, Lpt)                    :- getLpt(L, M, Lpt).

getLi([], M, Li)                                :- Li = M.
getLi([(I, and(C, D)) | L], M, Li)              :- concat(M, [(I, and(C, D))], Y), getLi(L, Y, Li).
getLi([(_, _) | L], M, Li)                      :- getLi(L, M, Li).

getLu([], M, Lu)                                :- Lu = M.
getLu([(I, or(C, D)) | L], M, Lu)               :- concat(M, [(I, or(C, D))], Y), getLu(L, Y, Lu).
getLu([(_, _) | L], M, Lu)                      :- getLu(L, M, Lu).

getLs([], M, Ls, Lie, Lpt, Li, Lu)              :- Ls = M.
getLs([(I, C) | L], M, Ls, Lie, Lpt, Li, Lu)    :- ((member((I, C), Lie);
                                                    member((I, C), Lpt);
                                                    member((I, C), Li);
                                                    member((I, C), Lu)),
                                                    getLs(L, M, Ls, Lie, Lpt, Li, Lu));
                                                    concat(M, [(I, C)], Y),
                                                    getLs(L, Y, Ls, Lie, Lpt, Li, Lu).

evolue((I, and(C, D)), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
    evolue_((I, C), Lie,    Lpt,  Li,    Lu,    Ls,    LieBis, Lpti, LiBis, LuBis, LsBis),
    evolue_((I, D), LieBis, Lpti, LiBis, LuBis, LsBis, Lie1,   Lpt1, Li1,   Lu1,   Ls1).
evolue((I, C), Lie, Lpt,    Li,   Lu,    Ls,    Lie1,  Lpt1,   Li1,  Lu1,   Ls1) :-
    evolue_((I, C), Lie,    Lpt,  Li,    Lu,    Ls,    Lie1,   Lpt1, Li1,   Lu1,   Ls1).

evolue_((I, C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1) :-
    (get_type(C, T), T == and,  Lie1 = Lie, Lpt1 = Lpt, Lu1 = Lu, Ls1 = Ls,   concat(Li,  [(I, C)], Li1));
    (get_type(C, T), T == or,   Lie1 = Lie, Lpt1 = Lpt, Li1 = Li, Ls1 = Ls,   concat(Lu,  [(I, C)], Lu1));
    (get_type(C, T), T == some, Li1 = Li,   Lpt1 = Lpt, Lu1 = Lu, Ls1 = Ls,   concat(Lie, [(I, C)], Lie1));
    (get_type(C, T), T == all,  Lie1 = Lie, Li1 = Li,   Lu1 = Lu, Ls1 = Ls,   concat(Lpt, [(I, C)], Lpt1));
    (get_type(C, T), T == not,  Lie1 = Lie, Li1 = Li,   Lu1 = Lu, Lpt1 = Lpt, concat(Ls,  [(I, C)], Ls1));
    (cnamea(C),                 Lie1 = Lie, Lpt1 = Lpt, Lu1 = Lu, Li1 = Li,   concat(Ls,  [(I, C)], Ls1)).

test_clash([(I, C) | L], Li) :- member((I, not(C)), Li); test_clash(L, Li).

printStates(Lie, Lpt, Li, Lu, Ls) :-
    write('\n\n===Current state==='), nl,
    write('\nLie: '), write(Lie),
    write('\nLpt: '), write(Lpt),
    write('\nLi: '),  write(Li),
    write('\nLu: '),  write(Lu),
    write('\nLs: '),  write(Ls).

complete_some([(A, some(R, C)) | L], Lpt, Li, Lu, Ls, Abr) :-
    write('\nRègle : Some\n'),
    generer_random_Iname(B), generer_random_Iname(X),
    write('---Start---'), write(X), nl,
    concat([(A, some(R, C))], L, Lie),
    printStates(Lie, Lpt, Li, Lu, Ls),
    evolue((B, C), L, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    concat(Abr, [(A, B, R)], Abr1),
    nl, write('---End---'), write(X), nl,
    printStates(Lie1, Lpt1, Li1, Lu1, Ls1),
    (test_clash(Ls1, Ls1); resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr1)).

transformation_and(Lie, Lpt, [(A, and(C, D)) | L], Lu, Ls, Abr) :-
    write('\nRègle : And'),
    concat([(A, and(C, D))], L, Li),
    generer_random_Iname(X),
    write('---Start---'), write(X), nl,
    printStates(Lie, Lpt, Li, Lu, Ls),
    evolue((A, and(C, D)) , Lie, Lpt, L, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    write('---End---'),  write(X), nl,
    printStates(Lie1, Lpt1, Li1, Lu1, Ls1),
    (test_clash(Ls1, Ls1); resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr)).

transformation_or(Lie, Lpt, Li, [(A, or(C, D)) | L], Ls, Abr) :-
    write('\nRègle : Or'),
    (evolue((A, C) , Lie, Lpt, Li, L, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
    printStates(Lie1, Lpt1, Li1, Lu1, Ls1),
    (test_clash(Ls1, Ls1); resolution(Lie1, Lpt1, Li1, Lu1, Ls1, Abr)),
    evolue((A, D) , Lie, Lpt, Li, L, Ls, Lie2, Lpt2, Li2, Lu2, Ls2),
    printStates(Lie2, Lpt2, Li2, Lu2, Ls2),
    (test_clash(Ls2, Ls2); resolution(Lie2, Lpt2, Li2, Lu2, Ls2, Abr))).

deduction_all(Lie, [(A, all(R, C)) | L], Li, Lu, Ls, Abr) :-
    write('\nRègle : All'),
    getAbr(A, R, Abr, [], M), bc(C, M, L, Ls1),
    printStates(Lie, L, Li, Lu, Ls1),
    (test_clash(Ls1, Ls1); resolution(Lie, L, Li, Lu, Ls1, br)).

getAbr(_, _, [], L, Res)                :- Res = L.
getAbr(A, R,[(X, Y, S) | L], L, Res)    :- A == X, R == S
                                        , concat(L, [(A, Y, R)], M)
                                        , getAbr(A, R, L, M, Res)
                                        ; getAbr(A, R, L, L, Res).

bc(_, [], L, Res)              :- Res = L.
bc(C, [(_, B, _) | L], M, Res) :- evolue((B, C), L, _, _, _, M, _, _, _, _, N), bc(C, L, N, Res).

resolution(Lie, Lpt, Li, Lu, Ls, Abr)   :- complete_some(Lie, Lpt, Li, Lu, Ls, Abr)
                                        ; transformation_and(Lie, Lpt, Li, Lu, Ls, Abr)
                                        ; deduction_all(Lie, Lpt, Li, Lu, Ls, Abr)
                                        ; transformation_or(Lie, Lpt, Li, Lu, Ls, Abr).