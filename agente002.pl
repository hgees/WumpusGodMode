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
:-dynamic([flecha/1,direcao/1,seguras/1,angulo/1,vida/1,wumpus/1,posicao/1,mudacasa/1,ouro/1,casafrente/1,frente/1,casaanterior/1,visitadas/1,perigosas/1,casasegura/1]).

wumpusworld(pit3, 4). %tipo, tamanho

init_agent:-
    retractall(posicao(_)),
    retractall(caverna(_)),
    retractall(vida(_)),
    retractall(ouro(_)),
    retractall(flecha(_)),
    retractall(direcao(_)),
    retractall(wumpus(_)),
    retractall(casafrente(_)),
    retractall(frente(_)),
    retractall(casaanterior(_)),
    retractall(seguras(_)),
    retractall(visitadas(_)),
    %    retracatll(perigosas(_)),
    assert(posicao([1,1])), %agente inicia na casa [1,1]
    assert(caverna(sim)),  %agente esta na caverna
    assert(vida(ativo)), %agente esta vivo
    assert(ouro(0)), %agente inicia sem o ouro
    assert(flecha(1)), %agente inicia com uma flecha.
    assert(direcao(0)), %agente inicia na direcao 0 grau (virado para direirta)
    assert(wumpus(vivo)), %wumpus inicia vivo
    assert(casafrente([2,1])), %agente inicia virado para direita e na casa [1,1]
    assert(frente([2,1])), %informa a casa a frente do agente, dependendo de sua orientacao
    assert(casaanterior([0,1])),%início de casas anteriores
    assert(seguras([1,1])), %informa as casas que sao seguras
    assert(visitadas([1,1])). %informa as casas em que o agente ja esteve
    %    assert(perigosas([])). %informa as casas que oferecem risco ao agente


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
    frente(Af),
    write('A casa a frente e: '), %informa a casa a frente, dependendo da direcao do agente
    writeln(Af),
    casaanterior(Ca),
    write('A casa anterior era: '), %informa a ultima casa que o agente esteve
    writeln(Ca),
    seguras(Cs),
    write('Lista de casas seguras: '), %informa as casas que nao orferecem risco ao agente
    writeln(Cs),
    visitadas(Cv),
    write('Lista de casas visitadas: '), %informa as casas que o agente ja visitou
    writeln(Cv),
    %    perigosas(Cp),
    %    write('Lista de casas perigosas: '), %informa as casas perigosas
    %    writeln(Cp),
    ouro(O),
    write('Numero de ouro: '), %informa o numero de ouro do agente 
    writeln(O),
    wumpus(V),
    write('Saude do Wumpus: '), %informa a condicao do wumpus 
    writeln(V),
    flecha(F),
    write('Flechas disponiveis: '), %quantidade de flechas disponiveis para tiro 
    writeln(F).
%casas seguras
%casas perigosas (adicionar estas funcoes).

%definindo direcao do agente.
direcao(0). %agente esta virado para direita.
% 0-> direita, 90-> cima, 180-> esquerda, 270-> baixo.

mudadiresq :- %mudanca da direcao para a esquerda (angulo maior em relacao ao inicial)
    direcao(D0),
    D1 is (D0+90) mod 360,
    retractall(direcao(_)),
    assert(direcao(D1)).

mudadirdir :- %diminui o angulo da direcao
    direcao(D0),
    D1 is (D0-90) mod 360,
    retractall(direcao(_)),
    assert(direcao(D1)).

mudacasa :- %funcoes para calcular a posicao do agente a partir de sua direcao
    direcao(Angulo),
    Angulo == 0,
    posicao([X,Y]),
    X<4,
    retractall(casaanterior(_)),
    assert(casaanterior([X,Y])),
    X1 is X+1,
    retractall(posicao([_|_])),
    assert(posicao([X1,Y])),
    retractall(frente(_)),
    X2 is X+1,
    assert(frente([X2,Y])).

mudacasa :- %agente esta virado para cima
    direcao(Angulo),
    Angulo == 90,
    posicao([X,Y]),
    Y<4,
    retractall(casaanterior(_)),
    assert(casaanterior([X,Y])),
    Y1 is Y+1,
    retractall(posicao([_|_])),
    assert(posicao([X,Y1])),
    retractall(frente(_)),
    Y2 is Y+2,
    assert(frente([X,Y2])).

mudacasa :- %agente virado para a esquerda
    direcao(Angulo),
    Angulo == 180,
    posicao([X,Y]),
    X>1,
    retractall(casaanterior(_)),
    assert(casaanterior([X,Y])),
    X1 is X-1,
    retractall(posicao([_|_])),
    assert(posicao([X1,Y])),
    retractall(frente(_)),
    X2 is X-2,
    assert(frente([X2,Y])).

mudacasa :- %agente virado para baixo
    direcao(Angulo),
    Angulo == 270,
    posicao([X,Y]),
    Y>1,
    retractall(casaanterior(_)),
    assert(casaanterior([X,Y])),
    Y1 is Y-1,
    retractall(posicao([_|_])),
    assert(posicao([X,Y1])),
    retractall(frente(_)),
    Y2 is Y-2,
    assert(frente([X,Y2])).

mudacasa :- %antes do agente andar, ele salva a casa atual como a futura casa anterior
    posicao([X,Y]),
    retractall(casaanterior(_)),
    assert(casaanterior([X,Y])).

antes :- %funcao que calcula a casa anterior
    posicao([X,Y]),
    casaanterior([A,B]),
    X==A,
    Y==B,
    retractall(casaanterior(_)),
    assert(casaanterior([X,Y])).

casafrente :- %funcao para calcular casa a frente, dependendo da direcao do agente
    direcao(Angulo),
    Angulo == 0,
    frente([X,Y]),
    retractall([_]),
    X1 is X+1,
    Y1 is Y-1,
    assert(frente([X1,Y1])).

casafrente :-
    direcao(Angulo),
    Angulo == 90,
    frente([X,Y]),
    retractall([_]),
    X1 is X+1,
    Y1 is Y+1,
    assert(frente([X1,Y1])).

casafrente :-
    direcao(Angulo),
    Angulo == 180,
    frente([X,Y]),
    retractall([_]),
    X1 is X-1,
    Y1 is Y+1,
    assert(frente([X1,Y1])).

casafrente :-
    direcao(Angulo),
    Angulo == 270,
    frente([X,Y]),
    retractall([_]),
    X1 is X-1,
    Y1 is Y-1,
    assert(frente([X1,Y1])).

casasvisitadas :- %funcao que salva casas visitadas
   visitadas(V),
   posicao(At),
   delete(V,[At],B),
   append(B,[At],Fui),
   retractall(visitadas(_)),
   assert(visitadas(Fui)).

%casasperigosas :- %funcao para calcular casas que oferecem risco
%  perigosas(P),
%  adjacentes(A),
%  seguras(S),
%  posicao(C),
%  retractall(perigosas(_)),
%  subtract(A,S,P1),
%  append(C,P1,P2),
%  assert(perigosas(P4)).

casasegura([no,no,_,_,_]) :- 
    seguras(K),
    posicao([S,L]),
    direcao==0,
    Z is S + 1,
    S<4,
    append(K,[[Z,L]], F),
    retractall(seguras(_)),
    assert(seguras(F)).

casasegura([no,no,_,_,_]) :-
    seguras(K),
    posicao([S,L]),
    direcao==90,
    Z is L + 1,
    L<4,
    append(K,[[S,Z]],F),
    retractall(seguras(_)),
    assert(seguras(F)).

casasegura([no,no,_,_,_]) :-
    seguras(K),
    posicao([S,L],
    direcao==180,
    Z is S - 1,
    S>1,
    append(K,[[S,L]],F),
    retractall(seguras(_)),
    assert(seguras(F)).

casasegura([no,no,_,_,_]) :-
    seguras(K),
    posicao([S,L]),
    direcao==270,
    Z is L-1,
    L>1,
    append(K,[[S,Z]],F),
    retractall(seguras(_)),
    assert(seguras(F)).

decflecha:- %funcao para diminuir numero de flechas apos o tiro
    flecha(X0),
    X1 is X0-1,
    retractall(flecha(_)),
    assert(flecha(X1)).

%inteligencia do agente
coragem([no,no,no,no,no], goforward):- %vai pra frente se não sentir perigo  
    mudacasa,
    casasvisitadas.

coragem([_,_,no,yes,no], turnleft) :- %vira para a esquerda se trombar
    mudadiresq.

%coragem([_,yes,no,no,no],A)

coragem([_,_,yes,_,_], grab) :- %pega o ouro se sentir o brilho
    retractall(ouro(_)),
    assert(ouro(1)).

coragem([_,_,_,_,yes],_) :- %nao atirar quando ouvir o grito; wumpus morto
    retractall(wumpus(_)),
    assert(wumpus(morto)).

coragem([yes,no,no,no,_], shoot) :-  %atira em linha reta se sentir fedor, wumpus estiver vivo e tiver uma flecha
    wumpus(vivo),
    flecha(X),
    X\==0,
    decflecha,
    retractall(flecha(_)),
    assert(flecha(0)).

coragem([yes,_,_,_,yes], goforward):- %vai pra frente sesentir fedor e wumpus estiver morto
    mudacasa.

coragem([_,no,no,no,_], goforward):- %vai para frente se nao trombar, sentir o vento ou sentir o brilho
    wumpus(morto),
    mudacasa,
    casasvisitadas.

coragem([_,yes,_,no,_], turnleft) :- %dar meia volta ao sentir o vento
    mudadiresq,
    mudadiresq.

coragem([_,_,_,_,_], climb) :- %agente deve sair da caverna se estiver na casa [1,1] e estiver com o ouro
    posicao([1,1]),
    ouro(1).
coragem([_,_,_,_,_], climb) :- %agente sai da caverna se estiver na casa [1,1 e se wumpus estiver morto
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

