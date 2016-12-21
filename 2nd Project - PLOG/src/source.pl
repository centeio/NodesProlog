:-include('Database2.pl').
:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-set_prolog_flag(toplevel_print_options,
                  [quoted(true), portrayed(true), max_depth(0)]).


invited(Invited):-
        Invited = [_,_,_],
        domain(Invited,0,1),
        Countries = [1,2,1],
        Gen = [1,-1,1],
%        Topics = [1,2,3],
        distanceTab(Dists),
        length(Invited,Len),
        length(Sums,Len),
        calcDist(Invited,Dists,Sums),
        sum(Sums,#=,Total),
        scalar_product(Gen,Invited,#=,0),
%       sum(Invited,#=,NPalestras),
        alldif1(Invited,Countries),        
        labeling([maximize(Total)],Invited).

calcDist([],[],[]).

calcDist([I1|Is],[D1|Ds],[S1|Ss]):-
        (I1 #= 1) #=> scalar_product(D1,Is,#=,S1),
        (I1 #= 0) #=> S1 #=0,
        calcDist(Is,Ds,Ss).

alldif1([],[]).
alldif1([Inv|RInv],[Country|RCountries]):-
        alldif1_aux(Inv,RInv,Country,RCountries),
        alldif1(RInv,RCountries).

alldif1_aux(_,[],_,[]).
alldif1_aux(Inv,[Inv2|RInv],Country,[Country2|RCountries]):-
        (Inv#=1 #/\ Inv2#=1) #=> (Country #\= Country2),
         alldif1_aux(Inv,RInv,Country,RCountries).


               
