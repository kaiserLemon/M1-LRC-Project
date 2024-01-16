concat([], L1, L1).
concat([X | Y], L1, [X | L2])           :- concat(Y, L1, L2).

concept(C)                              :- cnamea(C).
concept(C)                              :- cnamena(C).
concept(not(C))                         :- concept(C).
concept(or(C, D))                       :- concept(C), concept(D).
concept(and(C, D))                      :- concept(C), concept(D).
concept(all(R, C))                      :- rname(R), concept(C).
concept(some(R, C))                     :- rname(R), concept(C).
instC(I, C)                             :- iname(I), concept(C).

verifTbox([(C, D) | L])                 :- cnamea(C), concept(D), verifTbox(L).
verifTbox([]).
verifAboxFunc([], _).
verifAboxFunc([(I, C) | L], inst)     :- inst(I, C), verifAboxFunc(L, inst).
verifAboxFunc([(I, J, R) | L], role)  :- instR(I, J, R), verifAboxFunc(L, role).
verifAbox(Abi, Abr) :- verifAboxFunc(Abi, inst), verifAboxFunc(Abr, role).

verifAutoref([C | L])                   :- equiv(C, E), (autoref(C, E, []); verifAutoref(L)).

autoref(C, not(D), L)                   :- autoref(C, D, L).
autoref(C, and(C, D), L)                :- autoref(C, C, L); autoref(C, D, L).
autoref(C, or(C, D), L)                 :- autoref(C, C, L), autoref(C, D, L).
autoref(C, all(_, D), L)                :- autoref(C, D, L).
autoref(C, some(_, D), L)               :- autoref(C, D, L).
autoref(C, C, L)                        :- member(C, L); cnamena(C), concat(L, [C], M), equiv(C, Z), autoref(C, Z, M).
pas_autoref(C, CGen, L)                 :- \+ autoref(C, CGen, L).

nnf(not(and(C, D)), or(CN, DN))         :- nnf(not(C), CN), nnf(not(D), DN), !.
nnf(not(or(C, D)), and(CN, DN))         :- nnf(not(C), CN), nnf(not(D), DN), !.
nnf(not(all(R, C)), some(R, CN))        :- nnf(not(C), CN), !.
nnf(not(some(R, C)), all(R, CN))        :- nnf(not(C), CN), !.
nnf(not(not(X)), Y)                     :- nnf(X, Y), !.
nnf(not(X), not(X))                     :- !.
nnf(and(C, D), and(CN, DN))             :- nnf(C, CN), nnf(D, DN), !.
nnf(or(C, D), or(CN, DN))               :- nnf(C, CN), nnf(D, DN), !.
nnf(some(R, C), some(R, CN))            :- nnf(C, CN), !.
nnf(all(R, C), all(R, CN))              :- nnf(C, CN), !.
nnf(X, X).

atomC(A, B, C, D, E, Res) :- atom_concat(A, B, X), atom_concat(X, C, Y), atom_concat(Y, D, Z), atom_concat(Z, E, Res).
atomC(A, B, C, D, Res) :- atom_concat(A, B, X), atom_concat(X, C, Y), atom_concat(Y, D, Res).
atomC(A, B, C, Res) :- atom_concat(A, B, X), atom_concat(X, C, Res).
atomC(A, B, Res) :- atom_concat(A, B, Res).

atomF(and(CGen, DGen), X)               :- atomF(CGen, Q), atomF(DGen, R)
                                        , term_string(Q, S), term_string(R, T)
                                        , atomC("and(", S, ", ", T, ")", U)
                                        , term_string(X, U).
atomF(or(CGen, DGen), X)                :- atomF(CGen, Q), atomF(DGen, R)
                                        , term_string(Q, S), term_string(R, T)
                                        , atomC("or(", S, ", ", T, ")", U)
                                        , term_string(X, U).
atomF(all(R, CGen), X)                  :- atomF(CGen, Q)
                                        , term_string(Q, S), term_string(R, T)
                                        , atomC("all(", T, ", ", S, ")", U)
                                        , term_string(X, U).
atomF(some(R, CGen), X)                 :- atomF(CGen, Q)
                                        , term_string(Q, S), term_string(R, T)
                                        , atomC("some(", T, ", ", S, ")", U)
                                        , term_string(X, U).
atomF(not(CGen), X)                     :- atomF(CGen, Q)
                                        , term_string(Q, S)
                                        , atomC("not(", S, ")", U)
                                        , term_string(X, U).
atomF(CGen, X)                          :- cnamena(CGen), equiv(CGen, X); X = CGen.

traitement_Tbox([], A, L)               :- L = A.
traitement_Tbox([(C, CGen) | L], A, N)  :- pas_autoref(C, CGen, []), atomF(CGen, Y), nnf(Y, X), concat(A, [(C, Y)], M), traitement_Tbox(L, M, N).

traitement_Abox([], A, L)               :- L = A.
traitement_Abox([(C, CGen) | L], A, N)  :- pas_autoref(C, CGen, []), atomF(CGen, Y), nnf(Y, X), concat(A, [(C, Y)], M), traitement_Abox(L, M, N).