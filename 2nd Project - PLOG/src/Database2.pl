distanceTab([[1,10,10,10,10],
             [10,1,20,10],
                [1,10,10],
                [10,1],
                [10]]).
           
distance(1, 2, 1).
distance(1, 3, 10).
distance(1, 6, 10).
distance(1, 5, 10).
distance(1, 4, 10).

distance(2, 3, 10).
distance(2, 6, 10).
distance(2, 5, 20).
distance(2, 4, 1).

distance(3, 6, 10).
distance(3, 5, 10).
distance(3, 4, 1).

distance(4, 5, 10).
distance(4, 6, 1).

distance(5, 6, 10).

night(100).

budget(3000).

genero(-1, 'Female').
genero(1, 'Male').

%speaker(nome, genero, topico, pais, precoviagem, preco noite/pack)
%speaker(rui, 1, ai, portugal, 200, 200). %400
%speaker(sofia, 0, ml, portugal, 200, 100). %300
%speaker(will, 1, websecurity, usa, 700, 1). %900
%speaker(sarra, 0, vr, tunisia, 500, 1). %600
%speaker(pierre, 1, remoteaccess, france, 100, 2). %200
%speaker(li, 0, cloushosting, china, 800, 1). %1000

speaker(1, 1, 1, 1, 200, 1). %400
speaker(2, -1, 2, 1, 200, 2). %300
speaker(3, 1, 3, 2, 700, 1). %800
speaker(4, -1, 4, 3, 500, 1). %600
speaker(5, 1, 5, 4, 100, 1). %300
speaker(6, -1, 6, 5, 800, 2). %900

nome(1, 'Rui').
nome(2, 'Sofia').
nome(3, 'Will').
nome(4, 'Sarra').
nome(5, 'Pierre').
nome(6, 'Li').

pais(1, 'Portugal').
pais(2, 'USA').
pais(3, 'Tunisia').
pais(4, 'France').
pais(5, 'China').

topic(1, 'Artificial Intelligence').
topic(2, 'Machine Learning').
topic(3, 'Web Security').
topic(4, 'Virtual Reality').
topic(5, 'Remote Access').
topic(6, 'Cloud Hosting').