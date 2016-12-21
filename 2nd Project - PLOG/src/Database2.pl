distanceTab([[1,10],
          [0,10],
          [0,0]]).
           
distance(1, 2, 0).
%distance(1, 3, 10).
distance(1, 6, 1).
%distance(1, industry40, 1).
distance(1, 5, 1).
distance(1, 4, 1).

%distance(2, 3, 10).
distance(2, 6, 1).
%distance(2, industry40, 1).
distance(2, 5, 1).
distance(2, 4, 1).

%distance(3, cloudhosting, 10).
%distance(3, industry40, 10).
%distance(3, 5, 10).
%distance(3, 4, 10).

%distance(cloudhosting, industry40, 10).
distance(6, 5, 1).
distance(6, 4, 1).

%distance(industry40, 5, 10).
%distance(industry40, 4, 10).

distance(5, 4, 1).

night(100).

budget(1000).


%speaker(nome, genero, topico, pais, precoviagem, preco noite/pack)
%speaker(rui, 1, ai, portugal, 200, 200). %400
%speaker(sofia, 0, ml, portugal, 200, 100). %300
%speaker(will, 1, websecurity, usa, 700, 1). %900
%speaker(sarra, 0, vr, tunisia, 500, 1). %600
%speaker(pierre, 1, remoteaccess, france, 100, 2). %200
%speaker(li, 0, cloushosting, china, 800, 1). %1000

speaker(1, 1, 1, 1, 200, 200). %400
speaker(2, 0, 2, 6, 200, 100). %300
speaker(3, 1, 3, 2, 700, 100). %800
speaker(4, 0, 4, 3, 500, 100). %600
speaker(5, 1, 5, 4, 100, 200). %300
speaker(6, 0, 6, 5, 800, 100). %900


