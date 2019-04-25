%-----------------------------------------------------------------------------
% Nome: Nelson A.G. Trindade
% Numero IST: 93743
%-----------------------------------------------------------------------------
:- consult(codigo_comum). %Acessar ficheiro disponibilizado
%:- consult(testes_publicos/puzzles_publicos).

%----------------------------  Funcoes Auxiliares  ----------------------------
%-----------------------------------------------------------------------------
% troca_num(Num, N_Num):
% Pega no Num e troca o valor do bit para N_Num.
%-----------------------------------------------------------------------------
troca_num(X,Y):- abs(X-1,Y), !.

%-----------------------------------------------------------------------------
% mesmo_tipo(A,B):
% Verdade se A e B forem do mesmo tipo, i.e, se ambos sao numeros ou variaveis
%-----------------------------------------------------------------------------
mesmo_tipo(A,B) :- var(A), !, var(B).
mesmo_tipo(A,B) :- number(A), !, number(B).

%-----------------------------------------------------------------------------
% numero_elementos(Lst, Bit, Num):
% Pega numa lista Lst e verifica quantos valores iguais ao Bit existe e
% retorna Num, que e o numero de vezes que Bit esta na lista.
%-----------------------------------------------------------------------------
conta_elementos(Lst, Bit, Num):-
    findall(X, (member(X,Lst), X==Bit), Bag),
    length(Bag, Num), !.

%-----------------------------------------------------------------------------
% cmp_filas(Lst1, Lst2):
% Compara se 2 listas tem os mesmos valores (lembrando que _==_ da false)
%-----------------------------------------------------------------------------
cmp_filas([],[]):- !.
cmp_filas([X|Fila1],[Y|Fila2]):-
  mesmo_tipo(X,Y), X==Y, !, cmp_filas(Fila1,Fila2).

%-----------------------------------------------------------------------------
% cmp_puzzles(Lst1, Lst2):
% Compara se 2 listas tem os mesmos valores (lembrando que _==_ da false)
%-----------------------------------------------------------------------------
cmp_fila_puzzle([],[]):- !.
cmp_fila_puzzle(X,[Y|R]):-
  \+cmp_filas(X,Y), !, cmp_fila_puzzle(X,R).


%-------------------------------  MAIN PROGRAM  -------------------------------
%-----------------------------------------------------------------------------
% aplica_R1_triplo(Triplo, N_Triplo):
%   Triplo e uma lista de 3 elementos, em que cada elemento e 0, 1, ou uma
% variavel, significa que N_Triplo e a lista resultante de aplicar a regra 1
% ao triplo Triplo.
%-----------------------------------------------------------------------------
%Casos de ter mais do que uma variavel
aplica_R1_triplo(Fila,Fila):-
  findall(X,(member(X,Fila), var(X)), Bag),
  length(Bag,Num), Num>=2, !.
%Caso de ter um 1 e um 0
aplica_R1_triplo(Fila,Fila):-
  conta_elementos(Fila,0,Num1),
  conta_elementos(Fila,1,Num2), Num1=:=Num2, !.
%Casos em que apenas tem uma variavel
aplica_R1_triplo([X,Y,Z],[N_aux,Y,Z]):- var(X), Y=:=Z, !, troca_num(Y,N_aux).
aplica_R1_triplo([X,Y,Z],[X,N_aux,Z]):- var(Y), X=:=Z, !, troca_num(X,N_aux).
aplica_R1_triplo([X,Y,Z],[X,Y,N_aux]):- var(Z), Y=:=X, !, troca_num(Y,N_aux).
%Casos que nao tenha uma variavel
aplica_R1_triplo([X,Y,Z],[X,Y,Z]):- X=:=Z, Y=\=X, !.
aplica_R1_triplo([X,Y,Z],[X,Y,Z]):- Y=:=Z, X=\=Y, !.
aplica_R1_triplo([X,Y,Z],[X,Y,Z]):- Y=:=X, Z=\=Y, !.

%-----------------------------------------------------------------------------
% aplica_R1_fila_aux(Fila, N_Fila):
%   Fila e uma fila (linha ou coluna) de um puzzle, significa que N_Fila e a
% fila resultante de aplicar a regra 1 a fila Fila, uma so vez.
%-----------------------------------------------------------------------------
aplica_R1_fila_aux(Fila,Fila):-
  length(Fila,Num), Num<3, !.
aplica_R1_fila_aux([X,Y,Z|Re],[X1|N_Fila_aux]):-
  aplica_R1_triplo([X,Y,Z],[X1,Y1,Z1]),
  aplica_R1_fila_aux([Y1,Z1|Re],N_Fila_aux), !.

%-----------------------------------------------------------------------------
% aplica_R1_fila(Fila, N_Fila):
%   Fila e uma fila (linha ou coluna) de um puzzle, significa que N_Fila e a
% fila resultante de aplicar a regra 1 a fila Fila.
%-----------------------------------------------------------------------------
aplica_R1_fila(Fila,Fila):-
  aplica_R1_fila_aux(Fila, N_Fila), cmp_filas(Fila,N_Fila), !.
aplica_R1_fila(Fila,Novo):-
  aplica_R1_fila_aux(Fila, N_fila), aplica_R1_fila(N_fila,Novo), !.

%-----------------------------------------------------------------------------
% aplica_R2_fila(Fila, N_Fila):         REFAZER
%   Fila e uma fila (linha ou coluna) de um puzzle, significa que N_Fila e a
% fila resultante de aplicar a regra 2 a fila Fila.
%-----------------------------------------------------------------------------
aplica_R2_fila(Fila,Fila):-
  numero_elementos(Fila,1,Num1),
  numero_elementos(Fila,0,Num2),
  length(Fila,Na), not(Num1>Na/2), not(Num2>Na/2),!.

aplica_R2_fila(Fila,N_Fila):-
  numero_elementos(Fila,0,Num),
  length(Fila,Na), Num=:=Na/2, !,
  aplica_R2_fila_aux(Fila,1,N_Fila).

aplica_R2_fila(Fila,N_Fila):-
  numero_elementos(Fila,1,Num),
  length(Fila,Na), Num=:=Na/2,
  aplica_R2_fila_aux(Fila,0,N_Fila), !.

%-----------------------------------------------------------------------------
% aplica_R1_R2_fila(Fila, N_Fila):
%   Fila e uma fila (linha ou coluna) de um puzzle, significa que N_Fila e a
% fila resultante de aplicar a regra 2 a fila Fila.
%-----------------------------------------------------------------------------
aplica_R1_R2_fila(Fila,N_Fila):-
  aplica_R1_fila(Fila,N_R1), aplica_R2_fila(N_R1,N_Fila), !.

%-----------------------------------------------------------------------------
% aplica_R1_R2_matriz(Puz, N_Puz):
%   Puz e um puzzle, significa que N_Puz e o puzzle resultante de aplicar o
% predicado aplica_R1_R2_fila, as linhas de Puz.
%-----------------------------------------------------------------------------
aplica_R1_R2_aux([],[]):- !.
aplica_R1_R2_aux([X|R],[Y|N_aux]):-
  aplica_R1_R2_aux(R,N_aux), aplica_R1_R2_fila(X,Y), !.

%-----------------------------------------------------------------------------
% aplica_R1_R2_puzzle(Puz, N_Puz):
%   Puz e um puzzle, significa que N_Puz e o puzzle resultante de aplicar o
% predicado aplica_R1_R2_fila, as linhas e as colunas de Puz, por esta ordem.
%-----------------------------------------------------------------------------
aplica_R1_R2_puzzle(Puz,N_Puz):-
  aplica_R1_R2_aux(Puz,N_aux), transpose(N_aux,N_aux1),
  aplica_R1_R2_aux(N_aux1,N_aux2), transpose(N_aux2,N_Puz), !.

%-----------------------------------------------------------------------------
% inicializa(Puz, N_Puz):
%   Puz e um puzzle, significa que N_Puz e o puzzle resultante de inicializar
% o puzzle Puz.
%-----------------------------------------------------------------------------
inicializa(Fila,Fila):-
  aplica_R1_R2_puzzle(Fila,N_Fila), cmp_filas(N_Fila,Fila), !.
inicializa(Fila,N_Puz):-
  aplica_R1_R2_puzzle(Fila,N_Fila), !, inicializa(N_Fila,N_Puz).

%-----------------------------------------------------------------------------
% verifica_R3(Puz):
%   No puzzle Puz todas as linhas sao diferentes entre si e todas as colunas
% sao diferentes entre si.
%-----------------------------------------------------------------------------
verifica_R3_aux([]):- !.
verifica_R3_aux([X|R]):-
  cmp_fila_puzzle(X,R), verifica_R3_aux(R), !.

verifica_R3(Puz):-
  verifica_R3_aux(Puz),
  transpose(Puz, N_Puz), verifica_R3_aux(N_Puz), !.






%-----------------------------------------------------------------------------
% substitui_var(Fila, Bit, N_fila):
%   Pega numa fila e substitui a primeira variavel por Bit. Utilizado tambem
% para introduzir uma variavel.
%-----------------------------------------------------------------------------
substitui_var([], [], _, 1) :- !.
substitui_var([A|Fila],[Bit|Fila],Bit, 1) :-
  var(A), !.
substitui_var([A|Fila],[A|N_Fila],Bit, Y1) :-
  number(A),
  substitui_var(Fila, N_Fila, Bit, Y), Y1 is Y+1,!.

%-----------------------------------------------------------------------------
% substitui_t_var(Fila, Bit, N_fila):
% Pega numa fila e substitui todas as variaveis por Bit
%-----------------------------------------------------------------------------
substitui_t_var([],[],_) :- !.

substitui_t_var([X|R], [X|N_aux], Bit):-
  number(X), substitui_t_var(R,N_aux,Bit),!.
substitui_t_var([X|R], [Bit|N_aux], Bit):-
  var(X), substitui_t_var(R, N_aux, Bit), !.
