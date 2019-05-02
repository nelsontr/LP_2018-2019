
%---------------------------------------------------\
%             Catarina Sousa - N.93695              |
%---------------------------------------------------/


:-[codigo_comum].

%      /--------------------------------\
%      |     PREDICADOS AUXILIARES      |
%      \--------------------------------/


%--------------------------------------------------------------------
%conta_elementos_1(Lista, N):
%   Devolve: N -> O numero de uns existentes na Lista.
%--------------------------------------------------------------------

conta_elementos_1(Lista, N) :-
    findall(X, (member(X,Lista), X == 1), Aux),
    length(Aux, N).


%--------------------------------------------------------------------
%conta_elementos_0(Lista, N):
%   Devolve: N -> O numero de zeros existentes na Lista.
%--------------------------------------------------------------------

conta_elementos_0(Lista, N) :-
    findall(X, (member(X,Lista), X == 0), Aux),
    length(Aux, N).


%---------------------------------------------------------------------
%trocar(Lista, Elem):
%   Devolve: Lista com todas as variaveis da Lista
%           unificadas com o Elem.
%---------------------------------------------------------------------
trocar([],_).
trocar([H | T], Elem) :-
    var(H),
    H = Elem,
    trocar(T, Elem).
trocar([H | T], Elem) :-
    number(H),
    trocar(T, Elem).


%------------------------------------------------------------------------------------
%lista_contida(Lista, Puzzle):
%   Devolve: True ou False
%   Descricao: Verifica se a Lista nao esta contida no Puzzle.
%------------------------------------------------------------------------------------
lista_contida(_,[]).
lista_contida(Fila, [Linha | RLista]) :-
    Fila \== Linha, !,
    lista_contida(Fila, RLista).
lista_contida(Fila, [Linha|_]) :-
    Fila == Linha, !, fail.


%-------------------------------------------------------------------------------------
%aplica_R1_R2_puzzle_aux(Puzzle, N_Puzzle):
%   Devolve N_Puzzle -> o resultante de aplicar as Regras 1 e 2 as linhas do Puzzle.
%-------------------------------------------------------------------------------------
aplica_R1_R2_puzzle_aux([], []).
aplica_R1_R2_puzzle_aux([Fila1 | Resto], [N_Fila1 | Resto1]) :-
    aplica_R1_R2_fila(Fila1, N_Fila1), !,
    aplica_R1_R2_puzzle_aux(Resto, Resto1), !.


%------------------------------------------------------------------------------------
%compara_elementos_linhas(Lista1, Lista2, Linha, Coluna, Posicoes, NPosicoes):
%   Devolve: NPosicoses -> Lista de posicoes onde a Lista1 e Lista2 nao sao
%           equivalentes.
%   Descricao: Predicado que compara a Lista1 com a Lista2 elemento a elemento.
%------------------------------------------------------------------------------------
compara_elementos_linhas([],[],_,_,Posicoes, Posicoes).

compara_elementos_linhas(Lista1, Lista2,_ ,_, Posicoes, Posicoes) :-
    Lista1 =@= Lista2, !.

compara_elementos_linhas([H1 | Resto1], [H2 | Resto2], L, C, Posicoes, NPosicoes) :-
    H1 \=@= H2,
    append(Posicoes, [(L, C)], NPosicoes_aux),
    C1 is C + 1, !,
    compara_elementos_linhas(Resto1, Resto2, L, C1, NPosicoes_aux, NPosicoes).

compara_elementos_linhas([H1 | Resto1], [H2 | Resto2], L, C, Posicoes, NPosicoes) :-
    H1 =@= H2,
    C1 is C + 1, !,
    compara_elementos_linhas(Resto1, Resto2, L, C1, Posicoes, NPosicoes).


%------------------------------------------------------------------------------------
%compara_elementos_colunas(Lista1, Lista2, Linha, Coluna, Posicoes, NPosicoes):
%   Devolve: NPosicoses -> Lista de posicoes onde a Lista1 e Lista2 nao sao
%           equivalentes.
%   Descricao: Predicado que compara a Lista1 com a Lista2 elemento a elemento.
%------------------------------------------------------------------------------------
compara_elementos_colunas([],[],_,_,Posicoes, Posicoes).

compara_elementos_colunas([H1 | Resto1], [H2 | Resto2], L, C, Posicoes, NPosicoes) :-
    H1 \=@= H2,
    append(Posicoes, [(C,L)], NPosicoes_aux),
    C1 is C + 1, !,
    compara_elementos_colunas(Resto1, Resto2, L, C1, NPosicoes_aux, NPosicoes).

compara_elementos_colunas([H1 | Resto1], [H2 | Resto2], L, C, Posicoes, NPosicoes) :-
    H1 =@= H2,
    C1 is C + 1, !,
    compara_elementos_colunas(Resto1, Resto2, L, C1, Posicoes, NPosicoes).


%------------------------------------------------------------------------------------
%encontra_var_linhas(Lista, Linha, Coluna, Posicoes, NPosicoes):
%   Devolve: NPosicoes.
%   Descricao: encontra a primeira variavel da Lista e devolve-a em NPosicoes.
%------------------------------------------------------------------------------------
encontra_var_linhas([],_,_,Posicoes, Posicoes).

encontra_var_linhas([El | _], L, C, Posicoes, NPosicoes) :-
    var(El), !,
    append(Posicoes, [(L, C)], NPosicoes).

encontra_var_linhas([El | RestoLinha], L, C, Posicoes, NPosicoes) :-
    number(El),
    C1 is C + 1, !,
    encontra_var_linhas(RestoLinha, L, C1, Posicoes, NPosicoes).


%-----------------------------------------------------------------------------------
%encontra_var_puzzle(Puzzle, Linha, Posicoes):
%   Devolve: Posicoes -> recorrendo ao predicadoencontra_var_linhas devolve
%           em Posicoes a primeira posicao do Puzzle que e uma variavel.
%-----------------------------------------------------------------------------------
encontra_var_puzzle([],_,Posicoes) :-
    Posicoes = [(1,1)], !.

encontra_var_puzzle(_,_,Posicoes) :-
    not(var(Posicoes)),
    Posicoes \== [], !.

encontra_var_puzzle([Lista1 | Resto1], L, NPosicoes) :-
    encontra_var_linhas(Lista1, L, 1, _, NPosicoes),
    L1 is L+1, !,
    encontra_var_puzzle(Resto1, L1, NPosicoes).


%-----------------------------------------------------------------------------------
%so_numeros_listas(Lista):
%   Devolve: True ou False
%   Descricao: Predicado que verifica se a Lista so contem numeros.
%-----------------------------------------------------------------------------------
so_numeros_listas([]).
so_numeros_listas(Lista) :-
    [El |_] = Lista,
    var(El), !,
    false.

so_numeros_listas(Lista) :-
    [El | Resto] = Lista,
    number(El), !,
    so_numeros_listas(Resto).


%-------------------------------------------------------------------------------------
%so_numeros_puzzle(Puzzle):
%   Devolve: True ou False.
%   Descricao: Predicado que, recorrendo ao predicado so_numeros_listas, verifica
%               se o Puzzle so contem numeros.
%-------------------------------------------------------------------------------------
so_numeros_puzzle([]).

so_numeros_puzzle(Puz) :-
    [Lista1 | Resto] = Puz,
    so_numeros_listas(Lista1),
    so_numeros_puzzle(Resto), !.



%      /--------------------------------\
%      |     PREDICADOS DO PROJETO      |
%      \--------------------------------/



%--------------------------------------------------------------------------------------
%aplica_R1_triplo(Triplo, R):
%   Devolve: R -> o triplo resultante de aplicar a Regra 1 do jogo ao Triplo, isto e,
%           nao existem 3 zeros ou 3 uns seguidos em nenhuma linha ou coluna.
%--------------------------------------------------------------------------------------
aplica_R1_triplo(Lista, R) :- Lista = [Y,X,Y], var(Y), number(X), !, R = Lista.
aplica_R1_triplo(Lista, R) :- Lista = [Y,Y,X], var(Y), number(X), !, R = Lista.
aplica_R1_triplo(Lista, R) :- Lista = [X,Y,Y], var(Y), number(X), !, R = Lista.
aplica_R1_triplo(Lista, R) :- Lista = [Y,Y,Y], var(Y), !, R = Lista.
aplica_R1_triplo([1,1,X], R) :- var(X), !, R = [1, 1, 0].
aplica_R1_triplo([0,0,X], R) :- var(X), !, R = [0, 0, 1].
aplica_R1_triplo([1,X,1], R) :- var(X), !, R = [1, 0, 1].
aplica_R1_triplo([0,X,0], R) :- var(X), !, R = [0, 1, 0].
aplica_R1_triplo([X,1,1], R) :- var(X), !, R = [0, 1, 1].
aplica_R1_triplo([X,0,0], R) :- var(X), !, R = [1, 0, 0].
aplica_R1_triplo([1,X,0], R) :- var(X), !, R = [1, X, 0].
aplica_R1_triplo([0,X,1], R) :- var(X), !, R = [0, X, 1].
aplica_R1_triplo(Lista, R) :- Lista \= [1,1,1], Lista \= [0,0,0], !, R = Lista.


%--------------------------------------------------------------------------------------
%aplica_R1_fila_aux(Fila, N_Fila):
%   Devolve: N_Fila -> o resultante de aplicar da Regra 1 a Fila do inicio ao fim,
%           uma so vez.
%--------------------------------------------------------------------------------------
aplica_R1_fila_aux(Fila, N_Fila) :-
    length(Fila, X),
    X == 3, !,
    aplica_R1_triplo(Fila, N_Fila).

aplica_R1_fila_aux(Fila, N_Fila) :-
    Fila = [X, Y, Z | [H | Resto]], !,
    aplica_R1_triplo([X, Y, Z], [X1,Y1,Z1]),
    aplica_R1_fila_aux([Y1, Z1, H|Resto], N_aux),
    N_Fila= [X1|N_aux].


%--------------------------------------------------------------------------------------
%aplica_R1_fila(Fila, N_Fila):
%   Devolve: N_Fila -> o resultante de aplicar a Regra 1 a Fila ate nao ser possivel
%           alterar mais nenhuma posicao.
%--------------------------------------------------------------------------------------
aplica_R1_fila(Fila, N_Fila) :-
    aplica_R1_fila_aux(Fila, Aux1),
    aplica_R1_fila_aux(Aux1, Aux2),
    Aux2 \== Aux1,
    aplica_R1_fila(Aux2, N_Fila), !.

aplica_R1_fila(Fila, N_Fila) :-
    aplica_R1_fila_aux(Fila, Aux1),
    aplica_R1_fila_aux(Aux1, N_Fila),
    N_Fila == Aux1, !.


%--------------------------------------------------------------------------------------
%aplica_R2_fila(Fila, N_Fila):
%   Devolve: N_Fila -> o resultante de aplicar a Regra 2 a Fila, isto e, todas as
%           linhas e colunas tem o mesmo numero de zeros e uns (metade do tamanho).
%--------------------------------------------------------------------------------------
aplica_R2_fila(Fila, N_Fila) :-
    length(Fila, NumElem),
    conta_elementos_1(Fila, Num1),
    Num1 < NumElem // 2,
    conta_elementos_0(Fila, Num2),
    Num2 < NumElem // 2, !,
    N_Fila = Fila.

aplica_R2_fila(Fila, N_Fila) :-
    length(Fila, NumElem),
    duplicate_term(Fila, N_Fila),
    conta_elementos_1(N_Fila, Num1),
    Num1 =:= (NumElem // 2),
    trocar(N_Fila, 0), !.

aplica_R2_fila(Fila, N_Fila) :-
    length(Fila, NumElem),
    duplicate_term(Fila, N_Fila),
    conta_elementos_0(N_Fila, Num1),
    Num1 =:= (NumElem // 2),
    trocar(N_Fila, 1), !.


%-------------------------------------------------------------------------------------
%aplica_R1_R2_fila(Fila,  N_Fila):
%   Devolve: N_Fila -> o resultante de aplicar as Regras 1 e 2 a Fila.
%-------------------------------------------------------------------------------------
aplica_R1_R2_fila(Fila, N_Fila) :-
    aplica_R1_fila(Fila, Aux1),
    aplica_R2_fila(Aux1, N_Fila), !.


%-------------------------------------------------------------------------------------
%aplica_R1_R2_puzzle(Puzzle, N_Puzzle):
%   Devolve: N_Puzzle -> o resultante de aplicar as Regras 1 e 2 a todas as
%           linhas e a todas as colunas do Puzzle.
%-------------------------------------------------------------------------------------
aplica_R1_R2_puzzle([],[]).
aplica_R1_R2_puzzle(Puz, N_Puz) :-
    aplica_R1_R2_puzzle_aux(Puz, Aux1),
    mat_transposta(Aux1, Aux2),
    aplica_R1_R2_puzzle_aux(Aux2, Aux3),
    mat_transposta(Aux3, N_Puz), !.


%-------------------------------------------------------------------------------------
%inicializa(Puz, N_Puz):
%   Devolve: N_Puz -> o resultante de aplicar as Regras 1 e 2 ao Puz ate nao ser
%           possivel mais alteracoes.
%-------------------------------------------------------------------------------------
inicializa(Puz, N_Puz) :-
    aplica_R1_R2_puzzle(Puz, Aux1),
    aplica_R1_R2_puzzle(Aux1, Aux2),
    Aux1 \== Aux2,
    inicializa(Aux2, N_Puz), !.

inicializa(Puz, N_Puz) :-
    aplica_R1_R2_puzzle(Puz, Aux1),
    aplica_R1_R2_puzzle(Aux1, N_Puz),
    N_Puz == Aux1, !.


%-------------------------------------------------------------------------------------
%verifica_R3(Puzzle):
%   Devolve: True se nao existirem linhas iguais entre si e colunas iguais entre si.
%-------------------------------------------------------------------------------------
verifica_R3([]).
verifica_R3([H1 | Puz]) :-
    lista_contida(H1, Puz), !,
    verifica_R3(Puz).


%-------------------------------------------------------------------------------------
%propaga_posicoes(Posicoes, Puz, N_Puz):
%   Devolve: N_Puz -> o resultante de propagar as posicoes alteradas em cada alteracao
%           feita.
%-------------------------------------------------------------------------------------
propaga_posicoes([],Puz,Puz).
propaga_posicoes([(Linha, Coluna) | RestoPosicoes], Puz, N_Puz) :-
    duplicate_term(Puz, Aux),
    nth1(Linha, Aux, ListaLinha),
    aplica_R1_R2_fila(ListaLinha, NovaLista),
    mat_muda_linha(Aux, Linha, NovaLista, PuzAux),
    verifica_R3(PuzAux),
    compara_elementos_linhas(ListaLinha, NovaLista, Linha, 1, RestoPosicoes, PosicoesAux),
    mat_transposta(PuzAux, PuzTransp),
    nth1(Coluna, PuzTransp, ListaColuna),
    aplica_R1_R2_fila(ListaColuna, NovaColuna),
    mat_muda_linha(PuzTransp, Coluna, NovaColuna, PuzTranspNovo),
    verifica_R3(PuzTranspNovo),
    compara_elementos_colunas(ListaColuna, NovaColuna, Coluna, 1, PosicoesAux, NPosicoes),
    mat_transposta(PuzTranspNovo, NPuz),
    propaga_posicoes(NPosicoes, NPuz, N_Puz), !.


%-------------------------------------------------------------------------------------
%resolve(Puz, Solucao):
%   Devolve: Solucao -> resultante de resolver o Puz segundo as regras necessarias.
%-------------------------------------------------------------------------------------
resolve(Puz,Puz) :-
    so_numeros_puzzle(Puz), !.

resolve(Puz, Sol) :-
    duplicate_term(Puz, PuzAux),
    inicializa(PuzAux, PuzIniciado),
    verifica_R3(PuzIniciado),
    encontra_var_puzzle(PuzIniciado, 1, Posicoes),
    (\+so_numeros_puzzle(PuzIniciado)
      -> [(Linha, Coluna)] = Posicoes,
          mat_muda_posicao(PuzIniciado, (Linha, Coluna), 0, PuzAlterado),
          propaga_posicoes(Posicoes, PuzAlterado, Puzpropagado);
    Puzpropagado=PuzIniciado),
    resolve(Puzpropagado, Sol), !.

resolve(Puz, Sol) :-
    duplicate_term(Puz, PuzAux),
    inicializa(PuzAux, PuzIniciado),
    verifica_R3(PuzIniciado),
    encontra_var_puzzle(PuzIniciado, 1, Posicoes),
    (\+so_numeros_puzzle(PuzIniciado)
      -> [(Linha, Coluna)] = Posicoes,
          mat_muda_posicao(PuzIniciado, (Linha, Coluna), 1, PuzAlterado),
          propaga_posicoes(Posicoes, PuzAlterado, Puzpropagado);
    Puzpropagado=PuzIniciado),
    resolve(Puzpropagado, Sol), !.
%
% resolve(Puz, Sol) :-
%     duplicate_term(Puz, PuzAux),
%     inicializa(PuzAux, Sol),
%     verifica_R3(Sol).
%     encontra_var_puzzle(Sol, 1,_), !.
