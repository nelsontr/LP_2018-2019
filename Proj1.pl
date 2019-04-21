%-----------------------------------------------------------------------------
% Nome: Nelson A.G. Trindade
% Numero IST: 93743
%-----------------------------------------------------------------------------
:- consult(codigo_comum). %Acessar ficheiro disponibilizado
:- consult(puzzles_publicos).
%----------------------------  Funcoes Auxiliares  ----------------------------
%-----------------------------------------------------------------------------
% troca_numero(Num, N_Num):
% Pega no Num e troca o valor do bit para N_Num.
%-----------------------------------------------------------------------------
troca_numero(Num,0):- Num=:=1, !.
troca_numero(Num,1):- Num=:=0, !.

%-----------------------------------------------------------------------------
% numero_elementos(Lst, Bit, Num):
% Pega numa lista Lst e verifica quantos valores iguais ao Bit existe e
% retorna Num, que e o numero de vezes que Bit esta na lista.
%-----------------------------------------------------------------------------
%Caso terminal
numero_elementos([],_,0):-!.

numero_elementos([X|R],Bit,Num) :-
  not(var(X)), X=:=Bit,
  numero_elementos(R,Bit,N1), Num is N1 + 1,!.
numero_elementos([_|R],Bit,Num) :- numero_elementos(R,Bit,Num),!.

%-----------------------------------------------------------------------------
% aplica_R2_fila_aux(Fila, Bit, N_fila):
% < Descricao >
%-----------------------------------------------------------------------------
%Caso terminal
aplica_R2_fila_aux([],_,[]):-!.

aplica_R2_fila_aux([X|R],Bit,[X|N_aux]):-
  not(var(X)), %Caso nao unifique, i.e.,caso nao seja uma variavel
  aplica_R2_fila_aux(R,Bit,N_aux), !.
aplica_R2_fila_aux([X|R],Bit,[Bit|N_aux]):-
  var(X),  %Caso seja uma variavel
  aplica_R2_fila_aux(R,Bit,N_aux), !.


%-------------------------------  MAIN PROGRAM  -------------------------------

%-----------------------------------------------------------------------------
% aplica_R1_triplo(Triplo, N_Triplo):
%   Triplo e uma lista de 3 elementos, em que cada elemento e 0, 1, ou uma
% variavel, significa que N_Triplo e a lista resultante de aplicar a regra 1
% ao triplo Triplo.
%-----------------------------------------------------------------------------
%Casos de ter mais do que uma variavel
aplica_R1_triplo([X,Y,Z],[X,Y,Z]):- var(X), var(Y), !.
aplica_R1_triplo([X,Y,Z],[X,Y,Z]):- var(Y), var(Z), !.
aplica_R1_triplo([X,Y,Z],[X,Y,Z]):- var(X), var(Z), !.

%Casos em que apenas tem uma variavel
aplica_R1_triplo([X,Y,Z],[N_aux,Y,Z]):- var(X), Y=:=Z, !, troca_numero(Y,N_aux).
aplica_R1_triplo([X,Y,Z],[X,Y,Z]):- var(X), !.

aplica_R1_triplo([X,Y,Z],[X,N_aux,Z]):- var(Y), X=:=Z, !, troca_numero(X,N_aux).
aplica_R1_triplo([X,Y,Z],[X,Y,Z]):- var(Y), !.

aplica_R1_triplo([X,Y,Z],[X,Y,N_aux]):- var(Z), Y=:=X, !, troca_numero(Y,N_aux).
aplica_R1_triplo([X,Y,Z],[X,Y,Z]):- var(Z), !.

%Casos que nao tenha uma variavel
aplica_R1_triplo([X,Y,Z],[X,Y,Z]):- X=:=Z, Y=\=X, !.
aplica_R1_triplo([X,Y,Z],[X,Y,Z]):- Y=:=Z, X=\=Y, !.
aplica_R1_triplo([X,Y,Z],[X,Y,Z]):- Y=:=X, Z=\=Y, !.

%-----------------------------------------------------------------------------
% aplica_R1_fila_aux(Fila, N_Fila):
%   Fila e uma fila (linha ou coluna) de um puzzle, significa que N_Fila e a
% fila resultante de aplicar a regra 1 a fila Fila, uma so vez.
%-----------------------------------------------------------------------------
%Caso terminal
aplica_R1_fila_aux(Fila,Fila):- length(Fila,Num),Num<3,!.

aplica_R1_fila_aux([X,Y,Z|Re],[X1|N_Fila_aux]):-
  aplica_R1_triplo([X,Y,Z],[X1,Y1,Z1]),
  append([Y1,Z1],Re,R),
  aplica_R1_fila_aux(R,N_Fila_aux), !.

%-----------------------------------------------------------------------------
% aplica_R1_fila(Fila, N_Fila):
%   Fila e uma fila (linha ou coluna) de um puzzle, significa que N_Fila e a
% fila resultante de aplicar a regra 1 a fila Fila.
%-----------------------------------------------------------------------------
%Caso terminal
aplica_R1_fila(Fila,Fila):- aplica_R1_fila_aux(Fila, N_fila), N_fila==Fila, !.

aplica_R1_fila(Fila,Novo):- aplica_R1_fila_aux(Fila, N_fila),
  aplica_R1_fila(N_fila,Novo), !.

%-----------------------------------------------------------------------------
% aplica_R2_fila(Fila, N_Fila):
%   Fila e uma fila (linha ou coluna) de um puzzle, significa que N_Fila e a
% fila resultante de aplicar a regra 2 a fila Fila.
%-----------------------------------------------------------------------------
aplica_R2_fila(Fila,N_Fila):- numero_elementos(Fila,0,Num),
  length(Fila,Na), Num=:=Na/2,
  aplica_R2_fila_aux(Fila,1,N_Fila), !.
aplica_R2_fila(Fila,N_Fila):- numero_elementos(Fila,1,Num),
  length(Fila,Na), Num=:=Na/2,
  aplica_R2_fila_aux(Fila,0,N_Fila), !.

aplica_R2_fila(Fila,Fila):-
  numero_elementos(Fila,1,Num1),numero_elementos(Fila,0,Num2),
  length(Fila,Na), not(Num1>Na/2), not(Num2>Na/2),!.

%-----------------------------------------------------------------------------
% aplica_R1_R2_fila(Fila, N_Fila):
%   Fila e uma fila (linha ou coluna) de um puzzle, significa que N_Fila e a
% fila resultante de aplicar a regra 2 a fila Fila.
%-----------------------------------------------------------------------------
aplica_R1_R2_fila(Fila,N_Fila):-
  aplica_R1_fila(Fila,N_R1),aplica_R2_fila(N_R1,N_Fila).

%-----------------------------------------------------------------------------
% aplica_R1_R2_puzzle(Puz, N_Puz):
%   Puz e um puzzle, significa que N_Puz e o puzzle resultante de aplicar o
% predicado aplica_R1_R2_fila, as linhas e as colunas de Puz, por esta ordem.
%-----------------------------------------------------------------------------

%REFAZER
aplica_R1_R2_matriz([],[]).
aplica_R1_R2_matriz([X|R],[Novo|N_aux]):-
  aplica_R1_R2_matriz(R,N_aux),
  aplica_R1_R2_fila(X,Novo).

aplica_R1_R2_puzzle(Puz,N_Puz):-
  aplica_R1_R2_matriz(Puz,N_aux), transpose(N_aux,N_aux1),
  aplica_R1_R2_matriz(N_aux1,N_aux2), transpose(N_aux2,N_Puz).

%-----------------------------------------------------------------------------
% inicializa(Puz, N_Puz):
%   Puz e um puzzle, significa que N_Puz e o puzzle resultante de inicializar
% o puzzle Puz.
%-----------------------------------------------------------------------------

%REFAZER
inicializa(Fila,Fila):- aplica_R1_R2_puzzle(Fila, N_fila), N_fila==Fila, !.

inicializa(Fila,Novo):- aplica_R1_R2_puzzle(Fila, N_fila),
  aplica_R1_R2_puzzle(N_fila,Novo), !.


%-----------------------------------------------------------------------------
% verifica_R3(Puz):
%   No puzzle Puz todas as linhas sao diferentes entre si e todas as colunas
% sao diferentes entre si.
%-----------------------------------------------------------------------------
room([],[]).
room([X|Lst1],[Y|Lst2]):- X==Y, room(Lst1,Lst2),!.
room([X|Lst1],[Y|Lst2]):- var(X),var(Y), room(Lst1,Lst2),!.

aux(_,[]).
aux(X,[Y|Z]):-
  not(member(X,Y)), aux(X,Z),!.
aux(X,[Y|Z]):-
  member(X,Y),room(X,Y),aux(X,Z),!.

verifica_R3([]).
verifica_R3([X|R]):-
  not(member(X,R)),verifica_R3(R),!.
verifica_R3([X|R]):-
  member(X,R), aux(X,R), verifica_R3(R),!.


%-----------------------------------------------------------------------------
% propaga_posicoes(Posicoes, Puz, N_Puz):
%   Posicoes e uma lista de posicoes e Puz e um puzzle, significa que N_Puz e
% o resultado de propagar, recursivamente, (as mudancas de) as posicoes de
% Posicoes.
%-----------------------------------------------------------------------------



%-----------------------------------------------------------------------------
% resolve(Puz,Sol):
%   O Puzzle Sol e (um)a solucao do puzzle Puz. Na obtencao da solucao, deve
% ser utilizado o algoritmo apresentado na Seccao 1.
%-----------------------------------------------------------------------------
