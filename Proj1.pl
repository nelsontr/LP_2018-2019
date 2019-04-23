%-----------------------------------------------------------------------------
% Nome: Nelson A.G. Trindade
% Numero IST: 93743
%-----------------------------------------------------------------------------
:- consult(codigo_comum). %Acessar ficheiro disponibilizado
%:- consult(testes_publicos/puzzles_publicos).

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
numero_elementos([],_,0):-!.

numero_elementos([X|R],Bit,Num) :-
  number(X), X=:=Bit,
  numero_elementos(R,Bit,N1), Num is N1 + 1,!.
numero_elementos([_|R],Bit,Num) :- numero_elementos(R,Bit,Num),!.

%-----------------------------------------------------------------------------
% aplica_R2_fila_aux(Fila, Bit, N_fila):
% < Descricao >
%-----------------------------------------------------------------------------
aplica_R2_fila_aux([],_,[]):-!.

aplica_R2_fila_aux([X|R],Bit,[X|N_aux]):-
  number(X), aplica_R2_fila_aux(R,Bit,N_aux), !.
aplica_R2_fila_aux([X|R],Bit,[Bit|N_aux]):-
  var(X), aplica_R2_fila_aux(R,Bit,N_aux), !.


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
    numero_elementos(Fila,0,Num1),numero_elementos(Fila,1,Num2),
    Num1=:=Num2, !.

%Casos em que apenas tem uma variavel
aplica_R1_triplo([X,Y,Z],[N_aux,Y,Z]):- var(X), Y=:=Z, !, troca_numero(Y,N_aux).
aplica_R1_triplo([X,Y,Z],[X,N_aux,Z]):- var(Y), X=:=Z, !, troca_numero(X,N_aux).
aplica_R1_triplo([X,Y,Z],[X,Y,N_aux]):- var(Z), Y=:=X, !, troca_numero(Y,N_aux).

%Casos que nao tenha uma variavel
aplica_R1_triplo([X,Y,Z],[X,Y,Z]):- X=:=Z, Y=\=X, !.
aplica_R1_triplo([X,Y,Z],[X,Y,Z]):- Y=:=Z, X=\=Y, !.
aplica_R1_triplo([X,Y,Z],[X,Y,Z]):- Y=:=X, Z=\=Y, !.

%-----------------------------------------------------------------------------
% aplica_R1_fila_aux(Fila, N_Fila):
%   Fila e uma fila (linha ou coluna) de um puzzle, significa que N_Fila e a
% fila resultante de aplicar a regra 1 a fila Fila, uma so vez.
%-----------------------------------------------------------------------------
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
aplica_R1_fila(Fila,Fila):-
  aplica_R1_fila_aux(Fila, N_fila), N_fila==Fila, !.

aplica_R1_fila(Fila,Novo):-
  aplica_R1_fila_aux(Fila, N_fila), aplica_R1_fila(N_fila,Novo), !.

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
% AUXILIAR
% aplica_R1_R2_matriz(Puz, N_Puz):
%   Puz e um puzzle, significa que N_Puz e o puzzle resultante de aplicar o
% predicado aplica_R1_R2_fila, as linhas de Puz.
%-----------------------------------------------------------------------------
aplica_R1_R2_matriz([],[]).

aplica_R1_R2_matriz([X|R],[Y|N_aux]):-
  aplica_R1_R2_matriz(R,N_aux), aplica_R1_R2_fila(X,Y).

%-----------------------------------------------------------------------------
% aplica_R1_R2_puzzle(Puz, N_Puz):
%   Puz e um puzzle, significa que N_Puz e o puzzle resultante de aplicar o
% predicado aplica_R1_R2_fila, as linhas e as colunas de Puz, por esta ordem.
%-----------------------------------------------------------------------------
aplica_R1_R2_puzzle(Puz,N_Puz):-
  aplica_R1_R2_matriz(Puz,N_aux), transpose(N_aux,N_aux1),
  aplica_R1_R2_matriz(N_aux1,N_aux2), transpose(N_aux2,N_Puz).

%-----------------------------------------------------------------------------
% inicializa(Puz, N_Puz):
%   Puz e um puzzle, significa que N_Puz e o puzzle resultante de inicializar
% o puzzle Puz.
%-----------------------------------------------------------------------------
inicializa(Fila,Fila):-
  aplica_R1_R2_puzzle(Fila, N_fila), N_fila==Fila, !.

inicializa(Fila,N_Puz):-
  aplica_R1_R2_puzzle(Fila, N_fila),
  aplica_R1_R2_puzzle(N_fila,Novo),
  inicializa(Novo,N_Puz), !.

%-----------------------------------------------------------------------------
% verifica_R3(Puz):
%   No puzzle Puz todas as linhas sao diferentes entre si e todas as colunas
% sao diferentes entre si.
%-----------------------------------------------------------------------------

% Compara se 2 listas tem os mesmos valores (lembrando que _==_ da false)
%Caso terminal
fila_igual([],[]).
fila_igual([X|Lst1],[Y|Lst2]):- X==Y, fila_igual(Lst1,Lst2),!.

% Compara o primeiro termo com cada fila na matriz
%Caso terminal
fila_igual_matriz(_,[]).
fila_igual_matriz(X,[Y|Z]):-
  not(fila_igual(X,Y)), fila_igual_matriz(X,Z),!.

% Pega no primeiro termo e compara com a matriz restante
%Caso terminal
verifica_R3_linha([]).
verifica_R3_linha([X|R]):-
  fila_igual_matriz(X,R), verifica_R3_linha(R),!.

verifica_R3(Puz):-
  verifica_R3_linha(Puz), transpose(Puz, N_Puz), verifica_R3_linha(N_Puz),!.

%-----------------------------------------------------------------------------
% propaga_posicoes(Posicoes, Puz, N_Puz):
%   Posicoes e uma lista de posicoes e Puz e um puzzle, significa que N_Puz e
% o resultado de propagar, recursivamente, (as mudancas de) as posicoes de
% Posicoes.
%-----------------------------------------------------------------------------
diff(_,_,[], [],[]):-!.
diff(X,Num, [A|Fila], [B|N_Fila],[(X,Num)|Lst]):-
  var(A), number(B),!, Num1 is Num +1,
  diff(X,Num1,Fila,N_Fila,Lst).

diff(X,Num, [A|Fila], [B|N_Fila],Lst):-
  var(A), var(B), !, Num1 is Num +1,
  diff(X,Num1,Fila,N_Fila,Lst).
diff(X,Num, [A|Fila], [B|N_Fila],Lst):-
  number(A), number(B), !, Num1 is Num +1,
  diff(X,Num1,Fila,N_Fila,Lst).


diff_m([],[],[],_):-!.
diff_m([A|R],[B|R1],L1,Cont):-
  Cont_aux is Cont + 1,
  diff(Cont_aux,1,A,B,Lst),
  diff_m(R,R1,Laux,Cont_aux),
  append(Lst,Laux,L1),!.
%-----------------------------------------

escolhe_fila(_,[],[],_):-!.
escolhe_fila(X,[Y|R],[Y|N_aux],Contador):-
  X=\=Contador, Contador_aux is Contador + 1,
  escolhe_fila(X,R,N_aux,Contador_aux), !.

escolhe_fila(X,[Y|R],[Y1|N_aux],Contador):-
  X=:=Contador, Contador_aux is Contador + 1,
  aplica_R1_R2_fila(Y,Y1),
  escolhe_fila(X,R,N_aux,Contador_aux), !.


propaga_posicoes([],Novo,Novo):-!.
propaga_posicoes([(X,Y)|R],Puz,N_Puz):-
  escolhe_fila(X,Puz,N_aux,1), transpose(N_aux, N_T_aux),
  escolhe_fila(Y,N_T_aux,N,1), transpose(N, Novo),
  diff_m(Puz,Novo,Lst,0),
  append(Lst,R,Lst1), propaga_posicoes(Lst1,Novo,N_Puz).


%-----------------------------------------------------------------------------
% resolve(Puz,Sol):
%   O Puzzle Sol e (um)a solucao do puzzle Puz. Na obtencao da solucao, deve
% ser utilizado o algoritmo apresentado na Seccao 1.
%-----------------------------------------------------------------------------
resolve(_,_).
