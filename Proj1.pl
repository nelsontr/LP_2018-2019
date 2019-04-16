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

aplica_R1_fila(Fila,N_fila):-
  aplica_R1_fila_aux(Fila, N_fila),
  aplica_R1_fila(N_fila,Novo), N_fila=Novo, !.
