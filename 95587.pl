% --------------------------------------//---------------------------------------

% Gustavo Almeida Aguiar 95587

% -------------------------------------------------------------------------------
:- [codigo_comum].                                                              
% -------------------------------------------------------------------------------
% palavras_ordenadas/2                                                          |
% palavras_ordenadas(Lst, Letras)                                               |
% palavras_ordenadas recebe uma lista de listas com palavras (Lst) e            |
% retorna uma lista ordenada contendo as letras de cada palavra (Pal)           |
% -------------------------------------------------------------------------------

obtem_letras_palavras(Palavras, Letras) :-
    maplist(atom_chars, Palavras, Letras_Aux),
    sort(Letras_Aux, Letras).

% -------------------------------------------------------------------------------
% espaco_fila/2                                                                 |
% espaco_fila(Fila, Espaco)                                                     |
% Recebe uma lista contendo uma fila (linha ou coluna) de uma grelha            |
% e Espaco eh um espaco de Fila                                                 |
% -------------------------------------------------------------------------------

espaco_fila(Fila, Espaco) :-
    espaco_fila(Fila, [], Espaco).

espaco_fila([], Ac, Ac) :-
    length(Ac, Comp),
    Comp >= 3.

espaco_fila([H | _], Ac, Ac) :-
    H == #,
    length(Ac, Comp),
    Comp >= 3.

espaco_fila([H | T], Ac, Espaco) :-
    H == #,
    length(Ac, Comp),
    Comp >= 3,
    espaco_fila(T, [], Espaco).

espaco_fila([H | T], Ac, Espaco) :-
    H == #,
    length(Ac, Comp),
    Comp < 3,
    espaco_fila(T, [], Espaco).

espaco_fila([H | T], Ac, Espaco) :-
    H \== #,
    append(Ac, [H], Novo_Ac),
    espaco_fila(T, Novo_Ac, Espaco).

% -------------------------------------------------------------------------------
% espacos_fila/2                                                                |                                                               
% espacos_fila(Fila, Espacos)                                                   |
% Espacos eh a lista que contem todos os espacos da lista Fila                  |
% -------------------------------------------------------------------------------

espacos_fila(Fila, Espacos) :-
    bagof(Espacos_Aux, espaco_fila(Fila, Espacos_Aux), Espacos), !.
% Caso do false do bagof
espacos_fila(_, []).

% -------------------------------------------------------------------------------
% espacos_puzzle/2                                                              |
% espacos_puzzle(Grelha, Espacos)                                               |
% Grelha eh uma grelha e Espacos eh a lista de espacos dessa grelha             |
% -------------------------------------------------------------------------------

espacos_puzzle(Grelha, Espacos) :-
    maplist(espacos_fila, Grelha, Espacos_Linha),
    % Alisa a 1 nivel
    alisa(Espacos_Linha, Linhas),
    mat_transposta(Grelha, TGrelha),
    maplist(espacos_fila, TGrelha, Espacos_Coluna),
    alisa(Espacos_Coluna, Colunas),
    append(Linhas, Colunas, Espacos).

% -------------------------------------------------------------------------------
% espacos_com_posicoes_comuns/3                                                 |                                                               
% espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)                           |
% Espacos eh uma lista de espacos e Esp eh um espaco                            |
% Esps_com eh a lista de espacos com variaveis em comum com Esp tirando Esp     |
% -------------------------------------------------------------------------------

espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    include(posicoes_comuns(Esp), Espacos, Espacos_Comuns),
    exclude(==(Esp), Espacos_Comuns, Esps_com).

posicoes_comuns(Esp, Espaco) :-
    include(pertence(Esp), Espaco, Espacos_Comuns),
    Espacos_Comuns \== []. 

% -------------------------------------------------------------------------------
% palavra_possivel_esp/4                                                        |
% palavra_possivel_esp(Pal, Esp, Espacos, Letras)                               |
% Pal eh uma lista de letras de uma palavra -> [P, a, l, a, v, r, a]            |
% Esp eh um espaco -> [P1, P2, P3]                                              |
% Espacos eh o conjunto de espacos do puzzle                                    |
% Letras eh o conjunto de letras de palavras                                    |
% Pal eh uma palavra possivel para Esp                                          |
% -------------------------------------------------------------------------------

verificar_comuns([], _).

verificar_comuns([Espaco | T], Letras) :-
    member(Palavra, Letras),
    unificaveis(Espaco, Palavra), !,
    verificar_comuns(T, Letras).

palavra_possivel_esp(Pal, Esp, Espacos, Letras) :-
    member(Pal, Letras),
    Esp = Pal,
    espacos_com_posicoes_comuns(Espacos, Esp, Comuns),
    verificar_comuns(Comuns, Letras).

% -------------------------------------------------------------------------------
% palavras_possiveis_esp/4                                                      |
% palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis)                  |
% Pals_Possiveis eh a lista ordenada de palavras possiveis para o Espaco Esp    |
% -------------------------------------------------------------------------------

palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis) :-
    findall(Palavras, 
        palavra_possivel_esp(Palavras, Esp, Espacos, Letras), Pals_Possiveis).

% -------------------------------------------------------------------------------
% palavras_possiveis/3                                                          |
% palavras_possiveis(Letras, Espacos, Pals_Possiveis)                           |
% Pals_Possiveis eh a lista de palavras possiveis                               |
% -------------------------------------------------------------------------------

palavras_possiveis(Letras, Espacos, Pals_Possiveis) :-
    % Chamar clausula iterativa que contem todos os espacos
    palavras_possiveis(Letras, Espacos, Espacos, Pals_Possiveis), !.

palavras_possiveis(_, [], _, []).

palavras_possiveis(Letras, [Esp | T], Espacos, [[Esp, Palavras] | T1]) :-
    palavras_possiveis_esp(Letras, Espacos, Esp, Palavras),
    palavras_possiveis(Letras, T, Espacos, T1).

% -------------------------------------------------------------------------------
% letras_comuns/2                                                               |
% letras_comuns(Lst_Pals, Letras_comuns)                                        |
% Lst_Pals eh uma lista de listas de letras -> [[d, i, a], [d, o, a]]           |   
% Letras_comuns eh uma lista de pares(Pos, Letra)                               |
% Letras_Comuns tem a letra Letra na posicao Pos em todas as letras da lista    |
% -------------------------------------------------------------------------------

letras_comuns(Lst_Pals, Letras_Comuns) :-
    mat_transposta(Lst_Pals, Letras_Aux),
    letras_comuns(1, Letras_Aux, Letras_Comuns).

letras_comuns(_, [], []).

letras_comuns(Indice, [Letras | T], [(Indice, Letra) | T1]) :-
    nth1(1, Letras, Letra),
    iguais(Letras, Letra), !,
    Indice_Inc is Indice + 1,
    letras_comuns(Indice_Inc, T, T1).

% Caso do corte -> letras diferentes naquele indice
letras_comuns(Indice, [_ | Letras], Letras_Comuns) :-
    Indice_Inc is Indice + 1,
    letras_comuns(Indice_Inc, Letras, Letras_Comuns).

% -------------------------------------------------------------------------------
% atribui_comuns/1                                                              |
% atribui_comuns(Pals_Possiveis)                                                |
% Pals_Possiveis eh uma lista de palavras                                       |
% possiveis, atualiza esta lista atribuindo a cada espaco                       |
% as letras comuns a todas as palavras possiveis para esse espaco               |
% -------------------------------------------------------------------------------

atribui_comuns([]).
atribui_comuns([[Espaco, Palavras] | T]) :-
    letras_comuns(Palavras, Comuns),
    unifica(Espaco, Comuns),
    atribui_comuns(T).

unifica(_, []).
unifica(Espaco, [(Indice, Letra) | T1]) :-
    nth1(Indice, Espaco, EspacoI),
    EspacoI = Letra,
    unifica(Espaco, T1).

% -------------------------------------------------------------------------------
% retira_impossiveis/2                                                          |
% retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis)                      |
% Pals_Possiveis eh uma lista de palavras possiveis                             |
% Novas_Pals_Possiveis eh Pals_Possiveis sem as palavras impossiveis de unificar|
% com os espacos                                                                |
% -------------------------------------------------------------------------------

retira_impossiveis([], []).

% Caso unitario da palavra
retira_impossiveis([[Espaco, Palavras] | T], [[Espaco, Palavras] | T1]) :-
    length(Palavras, 1), !,
    retira_impossiveis(T, T1).

% Caso haja mais que uma palavra no conjunto Palavras
retira_impossiveis([[Espaco, Palavras] | T], [[Espaco, Palavras_Aux] | T1]) :-
    include(espaco_une_palavra(Espaco), Palavras, Palavras_Aux),
    retira_impossiveis(T, T1).

espaco_une_palavra(Espaco, Palavra) :-
    include(unificaveis(Espaco), [Palavra], Aux),
    Aux \== [].

% -------------------------------------------------------------------------------
% obtem_unicas/2                                                                |
% obtem_unicas(Pals_Possiveis, Unicas)                                          |
% Pals_Possiveis eh uma lista de palavras possiveis                             |
% Unicas eh a lista de palavras unicas de Pals_Possiveis                        |
% -------------------------------------------------------------------------------

tamanhoum(Palavra) :-
    length(Palavra, 1).

obtem_unicas(Pals_Possiveis, Unicas) :-
    % Lista de palavras dos conjuntos Espaco-Palavras
    maplist(nth1(2), Pals_Possiveis, Palavras),
    include(tamanhoum, Palavras, Unicas_Aux),
    alisa(Unicas_Aux, Unicas).

% -------------------------------------------------------------------------------
% retira_unicas / 2                                                             |
% retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis)                           |
% Novas_Pals_Possiveis sao as Pals_Possiveis mas retirando de cada              |
% conjunto Palavra-Espaco as palavras q sao unicas                              |
% -------------------------------------------------------------------------------

retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis) :-
    obtem_unicas(Pals_Possiveis, PalsUnicas),
    tirarunicas(Pals_Possiveis, PalsUnicas, Novas_Pals_Possiveis).

tirarunicas([], _, []).

tirarunicas([[Esp, Lst_Pals] | T], PalsUnicas, [[Esp, Lst_Pals_Aux] | T1]) :-
    length(Lst_Pals, Comp),
    Comp > 1, !,
    subtract(Lst_Pals, PalsUnicas, Lst_Pals_Aux),
    tirarunicas(T, PalsUnicas, T1).

% Caso de palavras unicas
tirarunicas([[Espaco, Palavras] | T], PalsUnicas, [[Espaco, Palavras] | T1]) :-
    tirarunicas(T, PalsUnicas, T1).

% -------------------------------------------------------------------------------
% simplifica / 2                                                                |
% simplifica(Pals_Possiveis, Novas_Pals_Possiveis)                              |
% -------------------------------------------------------------------------------

simplifica(Pals_Possiveis, Novas_Pals_Possiveis) :-
    atribui_comuns(Pals_Possiveis),
    retira_impossiveis(Pals_Possiveis, Pals_Possiveis_Imp),
    retira_unicas(Pals_Possiveis_Imp, Pals_Possiveis_Unicas),
    Pals_Possiveis \== Pals_Possiveis_Unicas, !, %se nao houve alteracoes acabou
    simplifica(Pals_Possiveis_Unicas, Novas_Pals_Possiveis).

simplifica(Pals_Possiveis, Pals_Possiveis) :-
    atribui_comuns(Pals_Possiveis).

% -------------------------------------------------------------------------------
% inicializa/2                                                                  |
% inicializa(Puz, Pals_Possiveis)                                               |
% -------------------------------------------------------------------------------

inicializa([Palavras, Grelha], Pals_Possiveis) :-
    obtem_letras_palavras(Palavras, Letras),
    espacos_puzzle(Grelha, Espacos),
    palavras_possiveis(Letras, Espacos, Pals_Possiveis_Aux),
    simplifica(Pals_Possiveis_Aux, Pals_Possiveis).

% -------------------------------------------------------------------------------
% escolhe_menos_alternativas/2                                                  |
% escolhe_menos_alternativas(Pals_Possiveis, Escolha)                           |
% Pals_Possiveis eh uma lista de palavras possiveis                             |
% Escolha eh o primeiro el de Palavras possiveis com minimo numero de palavras  |
% -------------------------------------------------------------------------------

escolhe_menos_alternativas(Pals_Possiveis, Escolha) :-
    exclude(umapalavra, Pals_Possiveis, Pals_Possiveis_Aux),
    % Selecionar as palavras dos conjuntos Espaco-Palavras
    maplist(nth1(2), Pals_Possiveis_Aux, Palavras),
    maplist(length, Palavras, Comprimentos),
    minimo(Comprimentos, Min),
    escolhe(Pals_Possiveis_Aux, Min, Escolha).

% Primeiro conjunto que tenha qtd palavras = min eh a escolha
escolhe([[Espaco, Palavras] | _], Min, Escolha) :-
    length(Palavras, Min), !,
    Escolha = [Espaco, Palavras].

escolhe([_ | T], Min, Escolha) :-
    escolhe(T, Min, Escolha). 

% -------------------------------------------------------------------------------
% experimenta_pal/3                                                             |
% experimenta_pal(Escolha, Pals_Possiveis, Novas_Pals_Possiveis)                |
% Escolha eh um dos elementos de Pals_Possiveis                                 |
% -------------------------------------------------------------------------------

experimenta_pal(Escolha, Pals_Possiveis, Novas_Pals_Possiveis) :-
    modifica(Escolha, Pals_Possiveis, Novas_Pals_Possiveis).

modifica(_, [], []).

% Caso em que o conjunto Espaco-Palavras eh a escolha
modifica([Espaco, Palavras], [H | T], [[Espaco, [Palavra]] | T1]) :-
    H == [Espaco, Palavras], !,
    member(Palavra, Palavras),
    Espaco = Palavra,
    modifica([Espaco, Palavras], T, T1).

% Caso do corte em que o conjunto Espaco-Palavras nao eh a escolha
modifica(Escolha, [H | T], [H | T1]) :-
    modifica(Escolha, T, T1).

% -------------------------------------------------------------------------------
% resolve_aux/2                                                                 |                                                       |
% resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis)                             |
% Pals_Possiveis eh uma lista de palavras possiveis                             |
% Novas_Pals_Possiveis eh o resultado                                           |
% -------------------------------------------------------------------------------

resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis) :-
    escolhe_menos_alternativas(Pals_Possiveis, Escolha), !,
    experimenta_pal(Escolha, Pals_Possiveis, Pals_Possiveis_Aux),
    simplifica(Pals_Possiveis_Aux, Pals_Possiveis_Simp),
    resolve_aux(Pals_Possiveis_Simp, Novas_Pals_Possiveis).

resolve_aux(Pals_Possiveis, Pals_Possiveis).

% -------------------------------------------------------------------------------
% resolve/1                                                                     |
% resolve(Puz)                                                                  |
% -------------------------------------------------------------------------------

resolve(Puz) :-
    inicializa(Puz, Pals_Possiveis),
    resolve_aux(Pals_Possiveis, _).

% -------------------------------------------------------------------------------
%                                   AUXILIARES                                  |
% -------------------------------------------------------------------------------
% -------------------------------------------------------------------------------
% alisa/2                                                                       |
% alisa(Lst, Res)                                                               |
% Res eh Lst alisada a 1 nivel                                                  |
% -------------------------------------------------------------------------------

alisa([], []).
alisa([H | T], Alisado) :- is_list(H), alisa(T, T_Aux), !, append(H, T_Aux, Alisado).
alisa([H | T], [H | T1]) :- alisa(T, T1).

% -------------------------------------------------------------------------------
% pertence/2                                                                    |
% pertence(Lst, El)                                                             |
% Retorna True se El pertence a Lst e False caso contrario                      |
% -------------------------------------------------------------------------------


pertence([H | _], El) :- H == El, !.
pertence([_ | T], El) :- pertence(T, El).

% -------------------------------------------------------------------------------
% unificaveis/2                                                                 |
% unificaveis(X, Y)                                                             |
% Retorna True se X eh unificavel com Y e False caso contrario                  |
% -------------------------------------------------------------------------------

unificaveis(X, Y) :- \+ (X \= Y).

% -------------------------------------------------------------------------------
% iguais/1                                                                      | 
% iguais(Lst)                                                                   |
% Retorna True se os elementos de Lst sao todos iguais a Letra                  |
% -------------------------------------------------------------------------------

iguais([], _).
iguais([H | T], Letra) :-
    H == Letra,
    iguais(T, Letra).

% -------------------------------------------------------------------------------
% minimo/2                                                                      |
% Min eh o minimo de uma lista                                                  |
% -------------------------------------------------------------------------------

minimo([H], H).
minimo([H, H1 | T], Min) :-
   H =< H1,
   minimo([H | T], Min).
minimo([H, H1 | T], Min) :-
   H1 < H,
   minimo([H1 | T], Min).

% -------------------------------------------------------------------------------
% umapalavra/1                                                                  |
% umapalavra(EspPal)                                                            |
% Retorna true se o conjunto Espaco-Palavras tem apenas uma palavra em Palavras |
% -------------------------------------------------------------------------------

umapalavra([_, Palavras]) :-
    length(Palavras, 1).

% --------------------------------------//---------------------------------------