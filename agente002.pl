% Some simple test agents.
%
% To define an agent within the navigate.pl scenario, define:
%   init_agent
%   restart_agent
%   run_agent
%
% Currently set up to solve the wumpus world in Figure 6.2 of Russell and
% Norvig.  You can enforce generation of this world by changing the
% initialize(random,Percept) to initialize(fig62,Percept) in the
% navigate(Actions,Score,Time) procedure in file navigate.pl and then run
% navigate(Actions,Score,Time).

% Lista de Percepcao: [Stench,Breeze,Glitter,Bump,Scream]
% Traducao: [Fedor,Vento,Brilho,Trombada,Grito]
% Acoes possiveis:
% goforward - andar
% turnright - girar sentido horario
% turnleft - girar sentido anti-horario
% grab - pegar o ouro
% climb - sair da caverna
% shoot - atirar a flecha

% Copie wumpus1.pl e agenteXX.pl onde XX eh o numero do seu agente (do grupo)
% para a pasta rascunhos e depois de pronto para trabalhos
% Todos do grupo devem copiar para sua pasta trabalhos, 
% com o mesmo NUMERO, o arquivo identico.

% Para rodar o exemplo, inicie o prolog com:
% swipl -s agente007.pl
% e faca a consulta (query) na forma:
% ?- start.

:- load_files([wumpus3]).
:-dynamic([flecha/1,direcao/1,seguras/1,angulo/1,vida/1,wumpus/1,posicao/1,mudacasa/1]).

wumpusworld(pit3, 4). %tipo, tamanho

init_agent:-
    retractall(posicao(_,_)),
    retractall(caverna(_)),
    retractall(vida(_)),
    retractall(ouro(_)),
    retractall(flecha(_)),
    retractall(direcao(_)),
    retractall(wumpus(_)),
    assert(posicao([1,1])),
    assert(caverna(sim)),
    assert(vida(ativo)),
    assert(ouro(0)),
    assert(flecha(1)),
    assert(direcao(0)),
    assert(wumpus(1)).


% esta funcao permanece a mesma. Nao altere.
restart_agent :- 
    init_agent.

% esta e a funcao chamada pelo simulador. Nao altere a "cabeca" da funcao. Apenas o corpo.
% Funcao recebe Percepcao, uma lista conforme descrito acima.
% Deve retornar uma Acao, dentre as acoes validas descritas acima.
run_agent(Percepcao, Acao) :-
    write('Percebi: '), 
    writeln(Percepcao),
    posicao(Posicao),
    write('Posição atual: '),
    writeln(Posicao),
    coragem(Percepcao, Acao),
    adjacentes(Posicao, L),
    direcao(Direcao),
    write('Direcao: '),
    writeln(Direcao).

%definindo direcao do agente.
direcao(0). %agente esta virado para direita.
% 0 -> direita, 90 -> cima, 180 -> esquerda, 270 -> baixo.

mudadiresq :-
    direcao(D0),
    D1 is D0 + 90,
    D2 is D1 mod 360,
    retractall(direcao(_)),
    assert(direcao(D2)).

mudadirdir :-
    direcao(D0),
    D1 is D0 - 90,
    D2 is D1 mod 360,
    retractall(posicao(_)),
    assert(direcao(D2)).

mudacasa :-
    angulo(0),
    posicao([X,Y]),
    X<4,
    X1 is X+1,
    retractall(posicao([_|_])),
    assert(posicao([X1,Y])).

mudacasa :-
    angulo(90),
    posicao([X,Y]),
    Y<4,
    Y1 is Y+1,
    retractall(posicao([_|_])),
    assert(posicao([X,Y1])).

mudacasa :-
    angulo(180),
    posicao([X,Y]),
    X>1,
    X1 is X+1,
    retractall(posicao([_|_])),
    assert(posicao([X1,Y])).

mudacasa :-
    angulo(270),
    posicao([X,Y]),
    Y>1,
    Y1 is Y-1,
    retractall(posicao([_|_])),
    assert(posicao([X,Y1])).

mudacasa :-
    angulo(270),
    posicao([X,Y]),
    Y==1,
    Y1 is Y,
    retractall(posicao([_|_])),
    assert(posicao([X,Y1])).

casasegura([no,no,_,_,_]) :- 
    seguras(K),
    posicao([S,L]),
    angulo(0),
    Z is S + 1,
    S<4,
    append(K,[[Z,L]], F),
    retractall(seguras(_)),
    assert(seguras(F)).

casasegura([no,no,_,_,_]) :-
    seguras(K),
    posicao([S,L]),
    angulo(90),
    Z is L + 1,
    L<4,
    append(K,[[S,Z]],F),
    retractall(seguras(_)),
    assert(seguras(F)).

decflecha:-
    flecha(X0),
    X1 is X0-1,
    retractall(flecha(_)),
    assert(flecha(X1)).

%inteligencia do agente
coragem([_,_,no,no,_], goforward) :-
    mudadiresq,
    mudacasa.
coragem([no,no,no,no,no], goforward). %vai pra frente se não sentir perigo 
coragem([_,_,no,yes,no], turnleft) :- %vira para a esquerda se trombar
    mudadiresq.
%coragem([_,yes,no,no,no],A)
coragem([_,_,yes,_,_], grab). %pega o ouro se sentir o brilho
coragem([_,_,_,_,yes],_). %nao atirar quando ouvir o grito
coragem([yes,no,no,no,_], shoot) :-  %atira em linha reta se sentir fedor e tiver uma flecha
    flecha(X),
    X\==0,
    decflecha.
coragem([yes,_,_,_,yes], goforward). %vai pra frente sesentir fedor e wumpus estiver morto
coragem([yes,_,_,no,_], goforward).
coragem([_,yes,_,no,_], turnleft) :-
    mudadiresq,
    mudadiresq.


cima([H, T], L1):-
    T1 is T+1,
    L1=[H, T1].

baixo([H, T], L2):-
    T2 is T-1,
    L2=[H, T2].

esquerda([H, T], L3):-
    H2 is H-1,
    L3=[H2, T].

direita([H, T], L4):-
    H1 is H+1,
    L4=[H1, T].


adjacentes([H, T], L):-
    H\==1,
    H\==4,
    T\==1,
    T\==4,
    cima([H, T], L1),
    baixo([H, T], L2),
    esquerda([H, T], L3),
    direita([H, T], L4),
    L=[L1, L2, L3, L4],
    write('Adjacentes: '),
    writeln(L).

adjacentes([H, T], L):-
    H==1,
    T==1,
    cima([H, T], L1),
    direita([H, T], L4),
    L=[L1, L4],
    write('Adjacentes: '),
    writeln(L).

adjacentes([H, T], L):-
    H==4,
    T==1,
    cima([H, T], L1),
    esquerda([H, T], L3),
    L=[L1, L3],
    write('Adjacentes: '),
    writeln(L).

adjacentes([H, T], L):-
    H==1,
    T==4,
    direita([H, T], L4),
    baixo([H, T], L2),
    L=[L2, L4],
    write('Adjacentes: '),
    writeln(L).

adjacentes([H, T], L):-
    H==4,
    T==4,
    baixo([H, T], L2),
    esquerda([H, T], L3),
    L=[L2, L3],
    write('Adjacentes: '),
    writeln(L).

adjacentes([H, T], L):-
    H\==1,
    H\==4,
    T==1,
    esquerda([H, T], L3),
    direita([H, T], L4),
    cima([H, T], L1),
    L=[L1, L3, L4],
    write('Adjacentes: '),
    writeln(L).

adjacentes([H, T], L):-
    H\==1,
    H\==4,
    T==4,
    esquerda([H, T], L3),
    direita([H, T], L4),
    baixo([H, T], L2),
    L=[L2, L3, L4],
    write('Adjacentes: '),
    writeln(L).

adjacentes([H, T], L):-
    T\==1,
    T\==4,
    H==1,
    cima([H, T], L1),
    direita([H, T], L4),
    baixo([H, T], L2),
    L=[L1, L2, L4],
    write('Adjacentes: '),
    writeln(L).

adjacentes([H, T], L):-
    T\==1,
    T\==4,
    H==4,
    cima([H,T], L1),
    esquerda([H, T], L3),
    baixo([H, T], L2),
    L=[L1, L2, L3],
    write('Adjacentes: '),
    writeln(L).
