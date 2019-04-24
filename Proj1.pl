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
troca_numero(X,Y):- abs(X-1,Y), !.

%-----------------------------------------------------------------------------
% mesmo_tipo(A,B):
% Verdade se A e B forem do mesmo tipo, i.e, se ambos sao numeros ou variaveis
%-----------------------------------------------------------------------------
mesmo_tipo(A,B):-var(A), var(B), !.
mesmo_tipo(A,B):-number(A), number(B), !.

%-----------------------------------------------------------------------------
% numero_elementos(Lst, Bit, Num):
% Pega numa lista Lst e verifica quantos valores iguais ao Bit existe e
% retorna Num, que e o numero de vezes que Bit esta na lista.
%-----------------------------------------------------------------------------
numero_elementos([],_,0):-!.

numero_elementos([X|R],Bit,Num):-
  number(X), X=:=Bit,
  numero_elementos(R,Bit,N1), Num is N1 + 1, !.
numero_elementos([_|R],Bit,Num):- numero_elementos(R,Bit,Num), !.

%-----------------------------------------------------------------------------
% aplica_R2_fila_aux(Fila, Bit, N_fila):
% < Descricao >
%-----------------------------------------------------------------------------
aplica_R2_fila_aux([],_,[]):-!.

aplica_R2_fila_aux([X|R],Bit,[X|N_aux]):-
  number(X), aplica_R2_fila_aux(R,Bit,N_aux), !.
aplica_R2_fila_aux([X|R],Bit,[Bit|N_aux]):-
  var(X), aplica_R2_fila_aux(R,Bit,N_aux), !.

%-----------------------------------------------------------------------------
% escolhe_fila(Linha,Puz,N_Puz,Contador):
%   De acordo com a linha introduzida, escolhe_fila vai a essa fila, e ira
% aplicar a regra R1 e R2 a fila. Se nao for igual, aumenta o contador e a linha
% nao modificada vai se juntar a N_Puz.
%-----------------------------------------------------------------------------
escolhe_fila(_,[],[],_) :- !.

escolhe_fila(X,[Y|R],[Y|N_aux],Contador) :-
  X\==Contador, Contador_aux is Contador + 1,
  escolhe_fila(X,R,N_aux,Contador_aux), !.
escolhe_fila(X,[Y|R],[Y1|N_aux],Contador):-
  X=:=Contador, Contador_aux is Contador + 1,
  aplica_R1_R2_fila(Y,Y1),
  %Quando estiver na posicao introduzida, aplica a regra R1 e R2 a fila.
  escolhe_fila(X,R,N_aux,Contador_aux), !.

%-----------------------------------------------------------------------------
% pos_alteradas_fila(Coord-X,Coord-Y,Fila, N_Fila,Lst):
%   Vai verificar na fila quais as posicoes que foram alteradas e colocar na
% lista Lst. Neste caso, Coord-X vai ser constante e Coord-Y vai alterar-se.
%-----------------------------------------------------------------------------
pos_alteradas_fila(_,_,[], [],[]) :- !.
pos_alteradas_fila(X,Num, [A|Fila], [B|N_Fila],Lst) :-
  mesmo_tipo(A,B), !, Num1 is Num +1,
  pos_alteradas_fila(X,Num1,Fila,N_Fila,Lst).
pos_alteradas_fila(X,Num, [A|Fila], [B|N_Fila],[(X,Num)|Lst]) :-
  not(mesmo_tipo(A,B)), !, Num1 is Num +1,
  pos_alteradas_fila(X,Num1,Fila,N_Fila,Lst).

%-----------------------------------------------------------------------------
% pos_alteradas_matrix(Fila, N_Fila,Lst,Coord-X):
%   Vai verificar no Puzzle quais as posicoes que foram alteradas e colocar na
% lista Lst. Neste caso, Coord-X vai alterar-se, uma vez que vamos dar a funcao
% pos_alteradas_fila essa coordenada
%-----------------------------------------------------------------------------
pos_alteradas_matrix([],[],[],_) :- !.
pos_alteradas_matrix([A|R],[B|R1],L1,X_aux) :-
  X is X_aux + 1,
  pos_alteradas_fila(X,1,A,B,Lst),
  pos_alteradas_matrix(R,R1,Laux,X),
  append(Lst,Laux,L1), !.


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

aplica_R1_triplo(Fila,Fila):- %Caso de ter um 1 e um 0
  numero_elementos(Fila,0,Num1),
  numero_elementos(Fila,1,Num2), Num1=:=Num2, !.

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
fila_igual([],[]) :- !.
fila_igual([X|Lst1],[Y|Lst2]):-
  X==Y, fila_igual(Lst1,Lst2), !.

% Compara o primeiro termo com cada fila na matriz
fila_igual_puzzle(_,[]) :- !.
fila_igual_puzzle(X,[Y|Z]):-
  not(fila_igual(X,Y)), fila_igual_puzzle(X,Z), !.

% Pega no primeiro termo e compara com a matriz restante
verifica_R3_linha([]) :- !.
verifica_R3_linha([X|R]) :-
  fila_igual_puzzle(X,R), verifica_R3_linha(R),!.

verifica_R3(Puz) :-
  verifica_R3_linha(Puz), transpose(Puz, N_Puz),
  verifica_R3_linha(N_Puz), !.

%-----------------------------------------------------------------------------
% propaga_posicoes(Posicoes, Puz, N_Puz):
%   Posicoes e uma lista de posicoes e Puz e um puzzle, significa que N_Puz e
% o resultado de propagar, recursivamente, (as mudancas de) as posicoes de
% Posicoes.
%-----------------------------------------------------------------------------
propaga_posicoes([],Novo,Novo) :- !.
propaga_posicoes([(X,Y)|R],Puz,N_Puz):-
  escolhe_fila(X,Puz,N_aux,1), transpose(N_aux, N_T_aux),
  escolhe_fila(Y,N_T_aux,N,1), transpose(N, Novo),
  pos_alteradas_matrix(Puz,Novo,Lst,0),
  append(Lst,R,Lst1), propaga_posicoes(Lst1,Novo,N_Puz), !.

%-----------------------------------------------------------------------------
% resolve(Puz,Sol):
%   O Puzzle Sol e (um)a solucao do puzzle Puz. Na obtencao da solucao, deve
% ser utilizado o algoritmo apresentado na Seccao 1.
%-----------------------------------------------------------------------------
induz_digito([], [], _, 1) :- !.
induz_digito([A|Fila],[Bit|Fila],Bit, 1) :-
  var(A), !.
induz_digito([A|Fila],[A|N_Fila],Bit, Y1) :-
  number(A),
  induz_digito(Fila, N_Fila, Bit, Y), Y1 is Y+1,!.


induz_digito_m([],[],_,0,_):-!.

induz_digito_m([A|R1],[B|R2],Bit,X,Y1):-
  induz_digito(A,B,Bit,Y), length(A,Num),Y>Num,
  induz_digito_m(R1,R2,Bit,X1,Y1),X is X1+1.
induz_digito_m([A|R1],[B|R1],Bit,1,Y):-
  induz_digito(A,B,Bit,Y), length(A,Num),Y=<Num.


resolve([A|Puz],N_aux):-
  inicializa([A|Puz],N_Puz), verifica_R3(N_Puz),
  induz_digito_m(N_Puz,N_aux,0,X,_),
  length(A,Num),X==Num.

resolve([A|Puz],Novo):-
  inicializa([A|Puz],N_Puz), verifica_R3(N_Puz),
  induz_digito_m(N_Puz,N_aux,0,X,Y),
  propaga_posicoes([(X,Y)],N_aux,Sol),
  verifica_R3(Sol),resolve(Sol,Novo), !.

resolve([A|Puz],Novo):-
  inicializa([A|Puz],N_Puz), verifica_R3(N_Puz),
  induz_digito_m(N_Puz,N_aux,1,X,Y),
  propaga_posicoes([(X,Y)],N_aux,Sol),
  verifica_R3(Sol),resolve(Sol,Novo), !.

