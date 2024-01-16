premiere_etape(Tbox, Abi, Abr) :-
    setof((C, CGen), equiv(C, CGen), TboxBis),
    setof((I, J), inst(I, J), AbiBis),
    setof((I, J, R), instR(I, J, R), Abr),
    setof(X, cnamena(X), M),

    (verifTbox(TboxBis) -> write("Syntaxe TBox OK"); write("ERREUR : Syntaxe TBox")), nl,
    (verifAbox(AbiBis, Abr) -> write("Syntaxe ABox OK"); write("ERREUR : Syntaxe ABox")), nl,
    (\+verifAutoref(M) -> write("Référencement TBox OK"); write("ERREUR : Auto-référencement TBox")), nl,

    traitement_Tbox(TboxBis, [], Tbox),
    traitement_Abox(AbiBis, [], Abi).

deuxieme_etape(Abi, Abi1, Tbox) :- saisie_et_traitement_prop_a_demontrer(Abi, Abi1, Tbox).

troisieme_etape(Abi, Abr) :- triAbox(Abi, Lie, Lpt, Li, Lu, Ls),
                            write("\n******************************\n"),
                            resolution(Lie, Lpt, Li, Lu, Ls, Abr).

programme :-
    load_files("pt1.pl"),
    load_files("pt2.pl"),
    load_files("pt3.pl"),
    load_files("Boxs.pl"),

    premiere_etape(Tbox, Abi, Abr),
    deuxieme_etape(Abi, Abi1, Tbox),
    (troisieme_etape(Abi1, Abr) -> write("Feuilles ouvertes."); write("Feuilles fermées."))
    ,nl.

programme.