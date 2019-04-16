%-----------------------------------------------------------------------------
% Nome: Nelson A.G. Trindade
% Numero IST: 93743
%-----------------------------------------------------------------------------
:- consult(codigo_comum). %Acessar ficheiro disponibilizado

%----------------------------  Funcoes Auxiliares  ----------------------------
%-----------------------------------------------------------------------------
% troca_numero(Num, N_Num):
% Pega no Num e troca o valor do bit para N_Num.
%-----------------------------------------------------------------------------
troca_numero(Num,N_Num):- Num=:=1,!,N_Num=0.
troca_numero(Num,N_Num):- Num=:=0,!,N_Num=1.

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
% < Descriçao >
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
%   Triplo é uma lista de 3 elementos, em que cada elemento é 0, 1, ou uma
% variável, significa que N_Triplo é a lista resultante de aplicar a regra 1
% ao triplo Triplo.
%-----------------------------------------------------------------------------
%Casos de ter mais do que uma variavel
aplica_R1_triplo([X,Y,Z],N_Triplo):- var(X), var(Y), !, N_Triplo=[X,Y,Z].
aplica_R1_triplo([X,Y,Z],N_Triplo):- var(Y), var(Z), !, N_Triplo=[X,Y,Z].
aplica_R1_triplo([X,Y,Z],N_Triplo):- var(X), var(Z), !, N_Triplo=[X,Y,Z].

%Casos em que apenas tem uma variavel
aplica_R1_triplo([X,Y,Z],N_Triplo):- var(X), Y=:=Z, !,
  troca_numero(Y,N_aux), N_Triplo=[N_aux,Y,Z].
aplica_R1_triplo([X,Y,Z],N_Triplo):- var(X), N_Triplo=[X,Y,Z], !.

aplica_R1_triplo([X,Y,Z],N_Triplo):- var(Y), X=:=Z, !,
  troca_numero(X,N_aux), N_Triplo=[X,N_aux,Z].
aplica_R1_triplo([X,Y,Z],N_Triplo):- var(Y), N_Triplo=[X,Y,Z], !.

aplica_R1_triplo([X,Y,Z],N_Triplo):- var(Z), Y=:=X, !,
  troca_numero(Y,N_aux), N_Triplo=[X,Y,N_aux].
aplica_R1_triplo([X,Y,Z],N_Triplo):- var(Z), N_Triplo=[X,Y,Z], !.

%Casos que nao tenha uma variavel
aplica_R1_triplo([X,Y,Z],N_Triplo):- X=:=Z, Y=\=X, N_Triplo=[X,Y,Z],!.
aplica_R1_triplo([X,Y,Z],N_Triplo):- Y=:=Z, X=\=Y, N_Triplo=[X,Y,Z],!.
aplica_R1_triplo([X,Y,Z],N_Triplo):- Y=:=X, Z=\=Y, N_Triplo=[X,Y,Z],!.

%-----------------------------------------------------------------------------
% ->Versao Recursiva
% aplica_R1_fila_aux(Fila, N_Fila):
%   Fila é uma fila (linha ou coluna) de um puzzle, significa que N_Fila é a
% fila resultante de aplicar a regra 1 à fila Fila, uma só vez.
%-----------------------------------------------------------------------------
%Caso terminal
aplica_R1_fila_aux(Fila,Fila):- length(Fila,Num),Num<3,!.

aplica_R1_fila_aux([X,Y,Z|Re],[X1|N_Fila_aux]):-
  aplica_R1_triplo([X,Y,Z],[X1,Y1,Z1]),
  append([Y1,Z1],Re,R),
  aplica_R1_fila_aux(R,N_Fila_aux), !.

%-----------------------------------------------------------------------------
% ->Versao Recursiva
% aplica_R1_fila(Fila, N_Fila):
%   Fila é uma fila (linha ou coluna) de um puzzle, significa que N_Fila é a
% fila resultante de aplicar a regra 1 à fila Fila.
%-----------------------------------------------------------------------------
%Caso terminal
aplica_R1_fila(Fila,Fila):- aplica_R1_fila_aux(Fila, N_fila), N_fila==Fila, !.

aplica_R1_fila(Fila,Novo):- aplica_R1_fila_aux(Fila, N_fila),
  aplica_R1_fila(N_fila,Novo), !.

%-----------------------------------------------------------------------------
% aplica_R2_fila(Fila, N_Fila):
%   Fila é uma fila (linha ou coluna) de um puzzle, significa que N_Fila é a
% fila resultante de aplicar a regra 2 à fila Fila.
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
%   Fila é uma fila (linha ou coluna) de um puzzle, significa que N_Fila é a
% fila resultante de aplicar a regra 2 à fila Fila.
%-----------------------------------------------------------------------------
aplica_R1_R2_fila(Fila,N_Fila):-
  aplica_R1_fila(Fila,N_R1),aplica_R2_fila(N_R1,N_Fila).
