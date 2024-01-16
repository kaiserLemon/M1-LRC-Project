saisie_et_traitement_prop_a_demontrer(Abi, Abi1, Tbox) :-
    nl, write('Entrez le numero du type de proposition que vous voulez demontrer :'), nl, 
    write('1 Une instance donnee appartient a un concept donne.'), nl, 
    write('2 Deux concepts n"ont pas d"elements en commun(ils ont une intersection vide).')
    , nl, read(R), suite(R, Abi, Abi1, Tbox).

suite(1, Abi, Abi1, Tbox) :- acquisition_prop_type1(Abi, Abi1, Tbox), !.
suite(2, Abi, Abi1, Tbox) :- acquisition_prop_type2(Abi, Abi1, Tbox), !.
suite(R, Abi, Abi1, Tbox) :- write('\nCette reponse est incorrecte.\n')
                            , saisie_et_traitement_prop_a_demontrer(Abi, Abi1, Tbox).

acquisition_prop_type1(Abi, Abi1, Tbox) :-
    write('I '), read(I),
    write('C '), read(C),
    instC(I, C), atomF(not(C), Y), nnf(Y, X), concat(Abi, [(I, X)], Abi1).

generer_random_Iname(IName) :-
    random(0,100000, X),
    atom_concat('i', X, INameStr),
    term_string(IName, INameStr).

acquisition_prop_type2(Abi, Abi1, Tbox) :-
    generer_random_Iname(I),
    write('C '), read(C),
    write('D '), read(D),
    concept(C), concept(D), atomF(not(and(C, D)), Y), nnf(Y, X), concat(Abi, [(I, X)], Abi1).