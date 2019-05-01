%-----------------------------------------------------------------------------
% Nome: Nelson A.G. Trindade
% Numero IST: 93743
%-----------------------------------------------------------------------------
:- consult(codigo_comum). %Acessar ficheiro disponibilizado

%----------------------------  Funcoes Auxiliares  ----------------------------
%-----------------------------------------------------------------------------
% troca_num(Num, N_Num):
%   Pega no Num e troca o valor do bit para N_Num (0 para 1, 1 para 0).
%-----------------------------------------------------------------------------
troca_num(X,Y):- abs(X-1,Y), !.

%-----------------------------------------------------------------------------
% mesmo_tipo(A,B):
%   E verdade se A e B forem do mesmo tipo, i.e, se ambos sao numeros ou
% se ambos sao variaveis.
%-----------------------------------------------------------------------------
mesmo_tipo(A,B):- var(A), !, var(B).
mesmo_tipo(A,B):- number(A), !, number(B).

%-----------------------------------------------------------------------------
% conta_elementos(Fila, Bit, Num, Tamanho):
%   Pega numa Fila e retorna Num, que e o numero de vezes que Bit esta na lista,
% juntamente com o tamanho da Fila.
%-----------------------------------------------------------------------------
conta_elementos_Fila(Fila, Bit, Num, Tamanho):-
    findall(X, (member(X,Fila), X==Bit), Bag),
    length(Fila, Tamanho), length(Bag, Num), !.


conta_var_Fila(Fila,Num1):-
  findall(Y,(member(Y,Fila),var(Y)), Bag),
  length(Bag,Num1), !.

conta_var_Puz(Puz,Num1):-
  findall(Y,(member(X,Puz),member(Y,X),var(Y)),Bag),
  length(Bag,Num1), !.

%-----------------------------------------------------------------------------
% cmp_filas(Fila1, Fila2):
% Compara se 2 filas sao iguais, i.e, se sao do mesmo tipo e iguais.
% (lembrando que _3==_4 da false)
%-----------------------------------------------------------------------------
cmp_filas([],[]):- !.
cmp_filas([A|Fila1],[B|Fila2]):-
  mesmo_tipo(A,B), A==B, !,
  cmp_filas(Fila1,Fila2).

%-----------------------------------------------------------------------------
% cmp_fila_puzzle(Fila, Puz):
% Compara se 1 fila e igual as filas restantes do Puz.
%-----------------------------------------------------------------------------
cmp_fila_puzzle(_,[]):- !.
cmp_fila_puzzle(X,[Y|R]):-
  \+cmp_filas(X,Y), !, cmp_fila_puzzle(Y,R).

%-----------------------------------------------------------------------------
% cmp_puzzles(Fila, Puz):
% Compara se 2 Puzzles sao iguais, linha por linha.
%-----------------------------------------------------------------------------
cmp_puzzles([],[]):-!.
cmp_puzzles([X|Puzzle1],[Y|Puzzle2]):-
  cmp_filas(X,Y), cmp_puzzles(Puzzle1,Puzzle2),!.

%-----------------------------------------------------------------------------
% substitui_var(Fila, N_Fila, Bit, Coord-Y):
%   Retorna N_Fila onde sera Fila substituindo a primeira variavel por Bit.
% Utilizado tambem para modificar so quando chegar a Coord-Y.
%-----------------------------------------------------------------------------
substitui_var([], [], _) :- !.
substitui_var([A|Fila],[Bit|Fila],Bit) :-
  var(A), !.
substitui_var([A|Fila],[A|N_Fila],Bit) :-
  substitui_var(Fila, N_Fila, Bit), !.

%-----------------------------------------------------------------------------
% substitui_t_var(Fila, N_Fila, Bit):
% Pega numa Fila e retorna N_Fila, que e a substituicao de todas as variaveis
% por Bit.
%-----------------------------------------------------------------------------
substitui_t_var(Fila,N_Fila,Bit):-
	substitui_var(Fila,N_Fila,Bit), cmp_filas(Fila,N_Fila),!.
substitui_t_var(Fila,N_Fila,Bit):-
  substitui_var(Fila,Aux,Bit),
  substitui_t_var(Aux,N_Fila,Bit), !.

%-----------------------------------------------------------------------------
% substitui_t_var(Fila, Bit, N_fila):
% Pega numa fila e substitui todas as variaveis por Bit
%-----------------------------------------------------------------------------
substitui_var_puzzle(_,[],[],0,_):-!.

substitui_var_puzzle(X,Puz,NPuz,Bit,Lst):-
  nth1(X,Puz,Fila), substitui_var(Fila,N_Fila,Bit),
  cmp_filas(Fila,N_Fila), Xaux is X+1,
  substitui_var_puzzle(Xaux,Puz,NPuz,Bit,Lst),!.

substitui_var_puzzle(X,Puz,NPuz,Bit,Lst):-
  nth1(X,Puz,Fila), substitui_var(Fila,N_Fila,Bit),
  \+cmp_filas(Fila,N_Fila), mat_muda_linha(Puz,X,N_Fila,N_Puz),
  escolhe_fila(X,N_Puz,NPuz,Lst),!.


%substitui_var_puzzle(1,Puz,NPuz,Bit,Lst).
/*substitui_var_puzzle([],[],_,0,_):-!.
substitui_var_puzzle([A|R1],[B|R2],Bit,X,Y1):-
  substitui_var(A,B,Bit), !,
  substitui_var_puzzle(R1,R2,Bit,X1,Y1), X is X1+1.
substitui_var_puzzle([A|R1],[B|R1],Bit,1,Y):-
  substitui_var(A,B,Bit), length(A,Num), Y=<Num, !.*/


%-----------------------------------------------------------------------------
% pos_alteradas_fila(Coord-X,Coord-Y,Fila, N_Fila,Lst):
%   Vai verificar na fila quais as posicoes que foram alteradas e colocar na
% lista Lst. Neste caso, Coord-X vai ser constante e Coord-Y vai alterar-se.
%-----------------------------------------------------------------------------
pos_alteradas_fila(_,_,[], [],[]) :- !.
pos_alteradas_fila(X,Y, [A|Fila], [B|N_Fila],Lst_aux) :-
  Y1 is Y +1,
  (\+mesmo_tipo(A,B), Lst_aux=[(X,Y)|Lst]; Lst_aux=Lst),
  pos_alteradas_fila(X,Y1,Fila,N_Fila,Lst), !.

%-----------------------------------------------------------------------------
% escolhe_fila(Linha,Puz,N_Puz,Contador):
%   De acordo com a linha introduzida, escolhe_fila vai a essa fila, e ira
% aplicar a regra R1 e R2 a fila. Se nao for igual, aumenta o contador e a linha
% nao modificada vai se juntar a N_Puz.
%-----------------------------------------------------------------------------
escolhe_fila(X,Puz,N_Puz,Lst):-
  nth1(X,Puz,Y), aplica_R1_R2_fila(Y,Y1),
  pos_alteradas_fila(X,1,Y,Y1,Lst),
  mat_muda_linha(Puz,X,Y1,N_Puz), !.

%-----------------------------------------------------------------------------
% verifica_R3_aux(Puz):
%   Verifica se a primeira linha de um Puz e igual a uma outra linha do mesmo
%-----------------------------------------------------------------------------
verifica_R3_aux([]):- !.
verifica_R3_aux([X|R]):-
  cmp_fila_puzzle(X,R), verifica_R3_aux(R), !.


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
  conta_elementos_Fila(Fila,0,1,_),
  conta_elementos_Fila(Fila,1,1,_), !.
%Casos em que apenas tem uma variavel ou nenhuma
aplica_R1_triplo(Fila,N_Fila):-
  (Bit=0, conta_elementos_Fila(Fila,0,2,_);
  Bit=1, conta_elementos_Fila(Fila,1,2,_)), !,
  troca_num(Bit,N_Bit), substitui_var(Fila,N_Fila,N_Bit).

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
  aplica_R1_fila_aux(Fila,N_Fila), cmp_filas(Fila,N_Fila), !.
aplica_R1_fila(Fila,N_Fila):-
  aplica_R1_fila_aux(Fila,N_aux), aplica_R1_fila(N_aux,N_Fila), !.

%-----------------------------------------------------------------------------
% aplica_R2_fila(Fila, N_Fila):       NOMES
%   Fila e uma fila (linha ou coluna) de um puzzle, significa que N_Fila e a
% fila resultante de aplicar a regra 2 a fila Fila.
%-----------------------------------------------------------------------------
aplica_R2_fila(Fila,Fila):-
  conta_elementos_Fila(Fila,0,Num1,Na), Num1<Na/2,
  conta_elementos_Fila(Fila,1,Num2,Na), Num2<Na/2, !.

aplica_R2_fila(Fila,N_Fila):-
  (Bit=0, conta_elementos_Fila(Fila,Bit,Num,Na), Num=:=Na/2;
  Bit=1, conta_elementos_Fila(Fila,Bit,Num,Na), Num=:=Na/2), !,
  troca_num(Bit,N_Bit), substitui_t_var(Fila,N_Fila,N_Bit).

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
  aplica_R1_R2_aux(Puz,N_aux1), mat_transposta(N_aux1,N_aux2),
  aplica_R1_R2_aux(N_aux2,N_aux3), mat_transposta(N_aux3,N_Puz), !.

%-----------------------------------------------------------------------------
% inicializa(Puz, N_Puz):
%   Puz e um puzzle, significa que N_Puz e o puzzle resultante de inicializar
% o puzzle Puz.
%-----------------------------------------------------------------------------
inicializa(Fila,Fila):-
  aplica_R1_R2_puzzle(Fila,N_Fila), cmp_puzzles(Fila,N_Fila), !.
inicializa(Fila,N_Puz):-
  aplica_R1_R2_puzzle(Fila,N_Fila), inicializa(N_Fila,N_Puz), !.

%-----------------------------------------------------------------------------
% verifica_R3(Puz):
%   No puzzle Puz todas as linhas sao diferentes entre si e todas as colunas
% sao diferentes entre si.
%-----------------------------------------------------------------------------
verifica_R3(Puz):-
  verifica_R3_aux(Puz), mat_transposta(Puz, N_Puz),
  verifica_R3_aux(N_Puz), !.

%-----------------------------------------------------------------------------
% propaga_posicoes(Posicoes, Puz, N_Puz):
%   Posicoes e uma lista de posicoes e Puz e um puzzle, significa que N_Puz e
% o resultado de propagar, recursivamente, (as mudancas de) as posicoes de
% Posicoes.
%-----------------------------------------------------------------------------
propaga_posicoes([],Novo,Novo) :- !.
propaga_posicoes([(X,Y)|R],Puz,N_Puz):-
  escolhe_fila(X,Puz,N_aux,Lst1), mat_transposta(N_aux, N_T_aux),
  escolhe_fila(Y,N_T_aux,N,Lst2), mat_transposta(N, Novo),
  append(Lst1,Lst2,Lst), append(Lst,R,L_aux),
  propaga_posicoes(L_aux,Novo,N_Puz), !.

%-----------------------------------------------------------------------------
% resolve(Puz,Sol):
%   O Puzzle Sol e (um)a solucao do puzzle Puz. Na obtencao da solucao, deve
% ser utilizado o algoritmo apresentado na Seccao 1.
%-----------------------------------------------------------------------------
resolve(Puz,Puz):-
  conta_var_Puz(Puz,Num1), Num1==0, !.

resolve(Puz,Novo):-
  inicializa(Puz,N_Puz),verifica_R3(N_Puz),
  substitui_var_puzzle(1,N_Puz,N_aux,0,Lst),
  propaga_posicoes(Lst,N_aux,Sol),
  resolve(Sol,Novo), !.
resolve(Puz,Novo):-
  inicializa(Puz,N_Puz),verifica_R3(N_Puz),
  substitui_var_puzzle(1,N_Puz,N_aux,1,Lst),
  propaga_posicoes(Lst,N_aux,Sol),
  resolve(Sol,Novo), !.
