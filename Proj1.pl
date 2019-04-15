%-----------------------------------------------------------------------------
% Nome: Nelson A.G. Trindade
% Numero IST: 93743
%-----------------------------------------------------------------------------
:- consult(codigo_comum).

%Funcoes auxiliares
%-----------------------------------------------------------------------------
% troca_numero(Num, N_Num):
% Pega no Num e troca o valor do bit para N_Num.
%-----------------------------------------------------------------------------
troca_numero(Num,N_Num):- Num=:=1,!,N_Num=0.
troca_numero(Num,N_Num):- Num=:=0,!,N_Num=1.
%troca_numero(Num,N_Num):- var(Num),!,N_Num=Num.



%----------------------  MAIN PROGRAM  ---------------------------

%-----------------------------------------------------------------------------
% aplica_R1_triplo(Triplo, N_Triplo):
% < Descricao >
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
% aplica_R1_fila_aux(Fila, N_Fila):
%   -Versao Recursiva
% < Descricao >
%-----------------------------------------------------------------------------

aplica_R1_fila_aux([],[]):- !.  %Caso Terminal
aplica_R1_fila_aux([X,Y,Z|Re],[X1,Y1,Z1|R]):- 
    aplica_R1_triplo([X,Y,Z],[X1,Y1,Z1]), aplica_R1_fila_aux(Re,R),!.