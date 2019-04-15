%-----------------------------------------------------------------------------
% Nelson A.G. Trindade
% Numero IST - 93743
%-----------------------------------------------------------------------------
:- consult(codigo_comum).

%Funcoes auxiliares
%-----------------------------------------------------------------------------
% troca_numero(Num, N_Num):
% Pega no Num e troca o valor do bit para N_Num.
%-----------------------------------------------------------------------------
troca_numero(Num,N_Num):- Num=:=1,!,N_Num=0.
troca_numero(Num,N_Num):- Num=:=0,!,N_Num=1.



%----------------------  MAIN PROGRAM  ---------------------------

%-----------------------------------------------------------------------------
% aplica_R1_triplo(Triplo, N_Triplo):
% < Descricao >
%-----------------------------------------------------------------------------

aplica_R1_triplo([X,Y,Z],N_Triplo):- var(Y), X=:=Z, !,
    troca_numero(X,N_aux), N_Triplo=[X,N_aux,Z].
%No caso de ser uma variavel e os outros distintos um do outro.
aplica_R1_triplo([X,Y,Z],N_Triplo):- var(Y), N_Triplo=[X,Y,Z], !. 

aplica_R1_triplo([X,Y,Z],N_Triplo):- var(X), Y=:=Z, !,
    troca_numero(Y,N_aux), N_Triplo=[N_aux,Y,Z].
%No caso de ser uma variavel e os outros distintos um do outro.
aplica_R1_triplo([X,Y,Z],N_Triplo):- var(X), N_Triplo=[X,Y,Z], !. 

aplica_R1_triplo([X,Y,Z],N_Triplo):- var(Z), Y=:=X, !,
    troca_numero(Y,N_aux), N_Triplo=[X,Y,N_aux].
%No caso de ser uma variavel e os outros distintos um do outro.
aplica_R1_triplo([X,Y,Z],N_Triplo):- var(Z), N_Triplo=[X,Y,Z], !. 

%Caso nao seja uma variavel
aplica_R1_triplo([X,Y,Z],N_Triplo):- X=:=Z, Y=\=X, N_Triplo=[X,Y,Z],!.
aplica_R1_triplo([X,Y,Z],N_Triplo):- Y=:=Z, X=\=Y, N_Triplo=[X,Y,Z],!.
aplica_R1_triplo([X,Y,Z],N_Triplo):- Y=:=X, Z=\=Y, N_Triplo=[X,Y,Z],!.