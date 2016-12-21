:-include('Database2.pl').
:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-set_prolog_flag(toplevel_print_options,
                  [quoted(true), portrayed(true), max_depth(0)]).


invited(Invited,NPalestras, NDias):-
        createSpeakerArrays(Speakers, Topics, Countries, Gen, Desloc, Aloj),
        createProximityMatrix(Topics, Dists),
        length(Speakers, Len),
        length(Invited, Len),
        domain(Invited,0,1),
        night(DPrice),
        calcDiasAloj(Aloj, NDias, DiasAloj),
        scalar_product(Desloc,Invited,#=,AllDesloc),
        scalar_product(DiasAloj,Invited,#=,AllAloj),
        budget(Budget),
        (AllDesloc + (AllAloj * DPrice)) #=< Budget,
        SumLen is Len-1,
        length(Sums,SumLen),
        calcDist(Invited,Dists,Sums),
        sum(Sums,#=,Total),
        scalar_product(Gen,Invited,#=,0),
        sum(Invited,#=,NPalestras),
        alldif1(Invited,Countries),
        statistics(walltime,_),    
        labeling([], Invited),
        statistics(walltime,[_,T]),
        write('Time: '), write(T), nl,
        fd_statistics,
        write('Topics distance: '), write(Total),nl,
        write('Despesa alojamento: '), write(AllAloj * DPrice),nl,
        write('Despesa deslocamento: '), write(AllDesloc), nl,
        printResult(Invited, Speakers, Topics, Countries, Gen, Desloc, DiasAloj, DPrice).

createProximityMatrix([], []).
createProximityMatrix([T1|Ts], [D1|Ds]) :-
        getDistLine(T1, Ts, D1),
        createProximityMatrix(Ts,Ds).

getDistLine(_, [], []).
getDistLine(T2, [T1|Ts], [D1|Ds]) :-
       distance(T2, T1, D1),
       getDistLine(T2, Ts, Ds). 

printResult([0|Is], [_|Ss], [_|Ts], [_|Cs], [_|Gs], [_|Ds], [_|As], DPrice) :-
        printResult(Is, Ss, Ts, Cs, Gs, Ds, As, DPrice).

printResult([1|Is], [S1|Ss], [T1|Ts], [C1|Cs], [G1|Gs], [D1|Ds], [A1|As], DPrice) :-
        nome(S1, Nome),
        pais(C1, Pais),
        genero(G1, Genero),
        topic(T1, Topic),
        Aloj is A1 * DPrice,
        write(Nome), write(' - '), write(Genero), write(' - '), write(Pais),
        write(' - '), write(Topic), write(' - '), write(D1), write(' - '), 
        write(Aloj), nl,
        printResult(Is, Ss, Ts, Cs, Gs, Ds, As, DPrice).
        
createSpeakerArrays(Speakers, Topics, Countries, Gen, Desloc, Aloj) :-
        findall(Speaker, speaker(Speaker,_,_,_,_,_), Speakers),
        findall(Topic, speaker(_,_,Topic,_,_,_), Topics),
        findall(Country, speaker(_,_,_,Country,_,_), Countries),
        findall(Gender, speaker(_,Gender,_,_,_,_), Gen),
        findall(D, speaker(_,_,_,_,D,_), Desloc),
        findall(A, speaker(_,_,_,_,_,A), Aloj).

calcDiasAloj([],_,[]).
calcDiasAloj([A1|As], NDias, [D1|Ds]) :-
        (A1 #= 1) #=> D1 #= NDias,
        (A1 #= 2) #=> D1 #= 1,
        calcDiasAloj(As, NDias, Ds).

calcDist(_,[[]|_],_).

calcDist([I1|Is],[D1|Ds],[S1|Ss]):-
        scalar_product(D1,Is,#=,Sum),
        S1 #= Sum * I1,
        calcDist(Is,Ds,Ss).


alldif1([],[]).
alldif1([Inv|RInv],[Country|RCountries]):-
        alldif1_aux(Inv,RInv,Country,RCountries),
        alldif1(RInv,RCountries).

alldif1_aux(_,[],_,[]).
alldif1_aux(Inv,[Inv2|RInv],Country,[Country2|RCountries]):-
        (Inv#=1 #/\ Inv2#=1) #=> (Country #\= Country2),
         alldif1_aux(Inv,RInv,Country,RCountries).