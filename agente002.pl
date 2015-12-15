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
:-dynamic([flecha/1,direcao/1,seguras/1,angulo/1,vida/1,wumpus/1,posicao/1,mudacasa/1,ouro/1]).

wumpusworld(pit3, 4). %tipo, tamanho

init_agent:-
    retractall(posicao(_)),
    retractall(caverna(_)),
    retractall(vida(_)),
    retractall(ouro(_)),
    retractall(flecha(_)),
    retractall(direcao(_)),
    retractall(wumpus(_)),
    assert(posicao([1,1])), %agente inicia na casa [1,1]
    assert(caverna(sim)),  %agente esta na caverna
    assert(vida(ativo)), %agente esta vivo
    assert(ouro(0)), %agente inicia sem o ouro
    assert(flecha(1)), %agente inicia com uma flecha.
    assert(direcao(0)), %agente inicia na direcao 0 grau (virado para direirta)
    assert(wumpus(vivo)). %wumpus inicia vivo


% esta funcao permanece a mesma. Nao altere.
restart_agent :- 
    init_agent.

% esta e a funcao chamada pelo simulador. Nao altere a "cabeca" da funcao. Apenas o corpo.
% Funcao recebe Percepcao, uma lista conforme descrito acima.
% Deve retornar uma Acao, dentre as acoes validas descritas acima.
run_agent(Percepcao, Acao) :-
    write('Percebi: '), %diz o que o agente percebeu (sentiu) (ok)
    writeln(Percepcao),
    posicao(P),
    write('Posição atual: '), %informa a casa atual do agente
    writeln(P),
    coragem(Percepcao, Acao),
    direcao(Direcao),
    write('Direcao: '), %informa a direcao do agente
    writeln(Direcao),
    adjacentes(P, L),
    write('Casas Adjacentes: '), %informa as casas adjacentes
    writeln(L),
    ouro(O),
    write('Numero de ouro: '), %informa o numero de ouro do agente (ok)
    writeln(O),
    wumpus(V),
    write('Saude do Wumpus: '), %informa a condicao do wumpus (ok)
    writeln(V),
    flecha(F),
    write('Flechas disponiveis: '), %quantidade de flechas disponiveis para tiro (ok)
    writeln(F).
%casa a frente
%casas visitadas
%casas seguras
%casas perigosas (adicionar estas funcoes).

%definindo direcao do agente.
direcao(0). %agente esta virado para direita.
% 0-> direita, 90-> cima, 180-> esquerda, 270-> baixo.

mudadiresq :- %mudanca da direcao para a esquerda (angulo maior em relacao ao inicial)
    direcao(D0),
    D1 is D0 + 90,
    D2 is D1 mod 360,
    retractall(direcao(_)),
    assert(direcao(D2)).

mudadirdir :- %diminui o angulo da direcao
    direcao(D0),
    D1 is D0 - 90,
    D2 is D1 mod 360,
    retractall(posicao(_)),
    assert(direcao(D2)).

mudacasa :- %funcoes para calcular a posicao do agente a partir de sua direcao
    direcao(Angulo),
    Angulo == 0,
    posicao([X,Y]),
    X<4,
    X1 is X+1,
    retractall(posicao([_|_])),
    assert(posicao([X1,Y])).

mudacasa :-
    direcao(Angulo),
    Angulo == 90,
    posicao([X,Y]),
    Y<4,
    Y1 is Y+1,
    retractall(posicao([_|_])),
    assert(posicao([X,Y1])).

mudacasa :-
    direcao(Angulo),
    Angulo == 180,
    posicao([X,Y]),
    X>1,
    X1 is X+1,
    retractall(posicao([_|_])),
    assert(posicao([X1,Y])).

mudacasa :-
    direcao(Angulo),
    Angulo == 270,
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

decflecha:- %funcao para diminuir numero de flechas apos o tiro (ok)
    flecha(X0),
    X1 is X0-1,
    retractall(flecha(_)),
    assert(flecha(X1)).

%inteligencia do agente
coragem([_,_,no,no,_], goforward) :- 
    mudadiresq,
    mudacasa([X]).

coragem([no,no,no,no,no], goforward). %vai pra frente se não sentir perigo  
    
coragem([_,_,no,yes,no], turnleft) :- %vira para a esquerda se trombar 
    mudadiresq.

%coragem([_,yes,no,no,no],A)
%
coragem([_,_,yes,_,_], grab) :- %pega o ouro se sentir o brilho (ok)
    retractall(ouro(_)),
    assert(ouro(1)).

coragem([_,_,_,_,yes],_) :- %nao atirar quando ouvir o grito; wumpus morto (ok)
    retractall(wumpus(_)),
    assert(wumpus(morto)).

coragem([yes,no,no,no,_], shoot) :-  %atira em linha reta se sentir fedor, wumpus estiver vivo e tiver uma flecha (ok)
    wumpus(vivo),
    flecha(X),
    X\==0,
    decflecha,
    retractall(flecha(_)),
    assert(flecha(0)).

coragem([yes,_,_,_,yes], goforward). %vai pra frente sesentir fedor e wumpus estiver morto

coragem([_,no,no,no,_], goforward). %vai para frente se nao trombar, sentir o vento ou sentir o brilho

coragem([_,yes,_,no,_], turnleft) :-
    mudadiresq,
    mudadiresq.

coragem([_,_,_,_,_], climb) :- %agente deve sair da caverna se estiver na casa [1,1] e estiver com o ouro. (ok)
    posicao([1,1]),
    ouro(1).
coragem([_,_,_,_,_], climb) :- %agente sai da caverna se estiver na casa [1,1 e se wumpus estiver morto. (ok)
    posicao([1,1]),
    wumpus(morto).

%funcoes para calcular as casas adjacentes
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
    L=[L1, L2, L3, L4].

adjacentes([H, T], L):-
    H==1,
    T==1,
    cima([H, T], L1),
    direita([H, T], L4),
    L=[L1, L4].

adjacentes([H, T], L):-
    H==4,
    T==1,
    cima([H, T], L1),
    esquerda([H, T], L3),
    L=[L1, L3].

adjacentes([H, T], L):-
    H==1,
    T==4,
    direita([H, T], L4),
    baixo([H, T], L2),
    L=[L2, L4].

adjacentes([H, T], L):-
    H==4,
    T==4,
    baixo([H, T], L2),
    esquerda([H, T], L3),
    L=[L2, L3].

adjacentes([H, T], L):-
    H\==1,
    H\==4,
    T==1,
    esquerda([H, T], L3),
    direita([H, T], L4),
    cima([H, T], L1),
    L=[L1, L3, L4].

adjacentes([H, T], L):-
    H\==1,
    H\==4,
    T==4,
    esquerda([H, T], L3),
    direita([H, T], L4),
    baixo([H, T], L2),
    L=[L2, L3, L4].

adjacentes([H, T], L):-
    T\==1,
    T\==4,
    H==1,
    cima([H, T], L1),
    direita([H, T], L4),
    baixo([H, T], L2),
    L=[L1, L2, L4].

adjacentes([H, T], L):-
    T\==1,
    T\==4,
    H==4,
    cima([H,T], L1),
    esquerda([H, T], L3),
    baixo([H, T], L2),
    L=[L1, L2, L3].

