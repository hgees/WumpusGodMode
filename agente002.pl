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
:-dynamic([flecha/1,
           direcao/1,
           seguras/1,
           wumpus/1,
           posicao/1,
           ouro/1,
           frente/1,
           casaanterior/1,
           visitadas/1,
           perigosas/1,
           alvo/1,
           giro/1]).

wumpusworld(pit3, 4). %tipo, tamanho

init_agent:-
    retractall(posicao(_)),
    retractall(ouro(_)),
    retractall(flecha(_)),
    retractall(direcao(_)),
    retractall(wumpus(_)),
    retractall(frente(_)),
    retractall(casaanterior(_)),
    retractall(seguras(_)),
    retractall(visitadas(_)),
    retractall(perigosas(_)),
    retractall(alvo(_)),
    retractall(giro(_)),
    assert(posicao([1,1])), %agente inicia na casa [1,1]
    assert(ouro(0)), %agente inicia sem o ouro
    assert(flecha(1)), %agente inicia com uma flecha.
    assert(direcao(0)), %agente inicia na direcao 0 grau (virado para direirta)
    assert(wumpus(vivo)), %wumpus inicia vivo
    assert(frente([2,1])), %informa a casa a frente do agente, dependendo de sua orientacao
    assert(casaanterior([])),%início de casas anteriores
    assert(seguras([[1,1]])), %informa as casas que sao seguras
    assert(visitadas([[1,1]])), %informa as casas em que o agente ja esteve
    assert(perigosas([])), %informa as casas que oferecem risco ao agente
    assert(alvo([])), %casa-alvo do agente
    assert(giro(0)). %aumenta quando o agente realiza turnleft ou turnright

% esta funcao permanece a mesma. Nao altere.
restart_agent :- 
    init_agent.

% esta e a funcao chamada pelo simulador. Nao altere a "cabeca" da funcao. Apenas o corpo.
% Funcao recebe Percepcao, uma lista conforme descrito acima.
% Deve retornar uma Acao, dentre as acoes validas descritas acima.
run_agent(Percepcao, Acao) :-
    write('Percebi: '), %diz o que o agente percebeu (sentiu) 
    writeln(Percepcao),
    posicao(P),
    write('Posição atual: '), %informa a casa atual do agente
    writeln(P),
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
    casafrente,
    write('A casa anterior era: '), %informa a ultima casa que o agente esteve
    writeln(Ca),
    seguras(Cs),
    write('Lista de casas seguras: '), %informa as casas que nao orferecem risco ao agente
    writeln(Cs),
    visitadas(Cv),
    versegura,
    write('Lista de casas visitadas: '), %informa as casas que o agente ja visitou
    writeln(Cv),
    casasvisitadas,
    perigosas(Cp),
    write('Lista de casas perigosas: '), %informa as casas perigosas
    writeln(Cp),
    alvo(Alvo),
    write('Meu alvo eh a casa: '),
    writeln(Alvo),
    ouro(O),
    write('Quantidade de ouro: '), %informa o numero de ouro do agente 
    writeln(O),
    wumpus(V),
    write('Condicao do Wumpus: '), %informa a saude do wumpus 
    writeln(V),
    flecha(F),
    write('Flechas disponiveis: '), %quantidade de flechas disponiveis para tiro 
    writeln(F),
    coragem(Percepcao, Acao).

%definindo direcao do agente.
% 0-> direita, 
% 90-> cima,
% 180-> esquerda,
% 270-> baixo.

roda:- %conta o numero de giros
    giro(G),
    G1 is G+1,
    retractall(giro(_)),
    assert(giro(G1)).

%inteligencia do agente002, com base em suas prioridades.
coragem([_,_,yes,_,_], grab):- %pega o ouro apos sentir o brilho
    retractall(ouro(_)),
    assert(ouro(1)),
    write('MAOE! Agora eu tenho barras de ouro!!!'),nl.

coragem([yes,no,no,no,_], shoot):- %atira em linha reta se sentir o fedor, wumpus estiver vivo e tiver uma flecha 
    wumpus(vivo),
    flecha(X),
    X\==0,
    decflecha,
    retractall(flecha(_)),
    assert(flecha(0)).

coragem([_,no,_,_,yes], _):- %nao atirar quando ouvir o grito; wumpus morto
    retractall(wumpus(_)),
    assert(wumpus(morto)), %adicionar casa do wumpus como segura.
    write('Acabou cao, agente002 chegou!!!'),nl,
    posicao(P),
    adjacentes(P, L),
    seguras(S),
    perigosas(Cp),
    append(L, S, S1),
    retractall(seguras(_)),
    assert(seguras(S1)),
    subtract(Cp, S1, P1),
    retractall(perigosas(_)),
    assert(perigosas(P1)).

coragem([_,_,_,_,_], climb):- %agente deve sair da caverna se estiver na casa [1,1] e tiver o ouro
    posicao([1,1]),
    ouro(1),
    write('Partiu!!!'),nl.
coragem([_,_,_,_,_], climb):- %agente sai da caverna se estiver na casa [1,1] e wumpus estiver morto
    posicao([1,1]),
    wumpus(morto),
    write('Aqui eh perigoso demais pra mim, vou nessa!'),nl.

coragem(_, goforward):- %impede que o agente002 entre em um loop infinito
    giro(G),
    G==2, 
    retractall(giro(_)),
    assert(giro(0)),
    mudacasa.

coragem([yes,no,no,no,no], turnleft):- %agnte vira ao sentir perigo
    mudadiresq,
    roda,
    verperigosas.

coragem([no,yes,no,no,no], turnleft):- %agente vira ao sentir perigo
    mudadiresq,
    roda,
    verperigosas.

coragem([no,no,no,no,no], goforward):- %se o agente nao sentir nenhuma ameaca e estiver proximo a uma casa segura ele anda
    mudacasa,
    versegura,
    target.

coragem([_,_,no,yes,no], turnleft):- %vira para a esquerda se trombar
    posicao([4,1]),
    direcao(Angulo),
    Angulo==0,
    mudadiresq,
    roda.
coragem([_,_,no,yes,no], turnright):-
    posicao([1,1]),
    direcao(Angulo),
    Angulo==180,
    mudadirdir,
    roda.
coragem([_,_,no,yes,no], turnright):-
    posicao([4,1]),
    direcao(Angulo),
    Angulo==270,
    mudadirdir,
    roda.
coragem([_,_,no,yes,no], turnleft):-
    posicao([1,1]),
    direcao(Angulo),
    Angulo=270,
    mudadiresq,
    roda.
coragem([_,_,no,yes,no], turnright):-
    posicao([1,4]),
    direcao(Angulo),
    Angulo==90,
    mudadirdir,
    roda.
coragem([_,_,no,yes,no], turnleft):-
    posicao([1,4]),
    direcao(Angulo),
    Angulo==180,
    mudadiresq,
    roda.
coragem([_,_,no,yes,no], turnleft):-
    posicao([4,4]),
    direcao(Angulo),
    Angulo==90,
    mudadiresq,
    roda.
coragem([_,_,no,yes,no], turnright):-
    posicao([4,4]),
    direcao(Angulo),
    Angulo==0,
    mudadirdir,
    roda.

coragem([_,no,no,no,_], goforward):- %agente anda se nao sentir brisa e o wumpus estiver morto
    wumpus(morto),
    mudacasa,
    versegura.
    
%agente deve andar pelas casas seguras @@@@@@@SEGURAS QUE AINDA N FORAM VISITADAS, E DEPOIS RETORNAR PELAS SEGURAS VISITADAS@@@@@@@@
coragem(_, Acao):-
    posicao([X,Y]),
    seguras(T),
    direcao(D),
    D==0,
    Z is X+1,
    member([Z,Y],T),
    acao(D,0,Acao).

coragem(_, Acao):-
    posicao([X,Y]),
    seguras(T),
    direcao(D),
    D==90,
    Z is Y+1,
    member([X,Z],T),
    acao(D,90,Acao).

coragem(_, Acao):-
    posicao([X,Y]),
    seguras(T),
    direcao(D),
    D==180,
    Z is X-1,
    member([Z,Y],T),
    acao(D,180,Acao).

coragem(_, Acao):-
    posicao([X,Y]),
    seguras(T),
    direcao(D),
    D==270,
    Z is Y-1,
    member([X,Z],T),
    acao(D,270,Acao).

coragem(_, goforward):-
    posicao([X,Y]),
    direcao(Angulo),
    seguras(S),
    Angulo==0,
    Z is X+1,
    member([Z,Y],S),
    mudacasa.

acao(D,Angulo2,turnleft):-
    D\==Angulo2,
    mudadadiresq.

acao(D,Angulo2,goforward):-
    D==Angulo2,
    mudacasa.

%calculando a acao, com base no issue criado em 15-12-2015 
%acoes para o agente andar para frente, com base em sua direcao.
%   (posicao,angulo,alvo,acao)
pense([X,Y], 0, [X2,Y], goforward):- %angulo=0
    X<X2,
    posicao([X,Y]),
    X<4,
    seguras(S),
    X2 is X+1,
    member([X2,Y],S),
    mudacasa.

pense([X,Y], 90, [X,Y2], goforward):- %angulo=90
    Y<Y2,
    posicao([X,Y]),
    Y<4,
    seguras(S),
    Y2 is Y+1,
    member([X,Y2],S),
    mudacasa.

pense([X,Y], 180, [X2,Y], goforward):- %angulo=180
    X>X2,
    posicao([X,Y]),
    X>1,
    seguras(S),
    X2 is X-1,
    member([X2,Y],S),
    mudacasa.

pense([X,Y], 270, [X,Y2], goforward):- %angulo=270
    Y>Y2,
    posicao([X,Y]),
    Y>1,
    seguras(S),
    Y2 is Y-1,
    member([X,Y2],S),
    mudacasa.


%para angulo=0 (virado para direita)
pense([X,Y], 0, [X2,Y], turnleft):-
    X>X2,
    mudadiresq.
pense([X,Y], 0, [X,Y2], turnright):-
    Y>Y2,
   mudadirdir.
pense([X,Y], 0, [X,Y2], turnleft):-
    Y<Y2,
    mudadiresq.

%para angulo=90 (virado para cima)
pense([X,Y], 90, [X2,Y], turnleft):-
    X>X2,
    mudadiresq.
pense([X,Y], 90, [X2,Y], turnright):-
    X<X2,
    mudadirdir.
pense([X,Y], 90, [X,Y2], turnleft):-
    Y>Y2,
    mudadiresq.

%para angulo=180 (virado para a esquerda)
pense([X,Y], 180, [X2,Y], turnleft):-
    X<X2,
    mudadiresq.
pense([X,Y], 180, [X,Y2], turnright):-
    Y<Y2,
    mudadirdir.
    
pense([X,Y], 180, [X,Y2], turnleft):-
    Y>Y2,
    mudadiresq.

%para angulo=270 (virado para baixo)
pense([X,Y], 270, [X2,Y], turnleft):-
    X<X2,
    mudadiresq.
pense([X,Y], 270, [X2,Y], turnright):-
    X>X2,
    mudadirdir.
pense([X,Y], 270, [X,Y2], turnleft):-
    Y<Y2,
    mudadiresq.



%funcoes de apoio
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
    posicao([X,Y]),
    X<4,
    retractall(frente(_)),
    X1 is X+1,
    assert(frente([X1,Y])).

casafrente :-
    direcao(Angulo),
    Angulo == 90,
    posicao([X,Y]),
    Y<4,
    retractall(frente(_)),
    Y1 is Y+1,
    assert(frente([X,Y1])).

casafrente :-
    direcao(Angulo),
    Angulo == 180,
    posicao([X,Y]),
    X>1,
    retractall(frente(_)),
    X1 is X-1,
    assert(frente([X1,Y])).

casafrente :-
    direcao(Angulo),
    Angulo == 270,
    posicao([X,Y]),
    Y>1,
    retractall(frente(_)),
    Y1 is Y-1,
    assert(frente([X,Y1])).

casasvisitadas :- %funcao que salva casas visitadas
   visitadas(V),
   posicao(At),
   delete(V,[At],B),
   append(B,[At],Fui),
   list_to_set(Fui,Reduz),
   retractall(visitadas(_)),
   assert(visitadas(Reduz)).

%funcao para calcular casas seguras
versegura:-
    casasegura0,
    casasegura90,
    casasegura180,
    casasegura270,
    tiraperigosas,
    reduz.

casasegura0:-
    seguras(S),
    posicao([X,Y]),
    X<4,
    Z is X+1,
    not(member([Z,Y], S)),
    append(S,[[Z,Y]],S1),
    retractall(seguras(_)),
    assert(seguras(S1)).
casasegura0.

casasegura90:-
    seguras(S),
    posicao([X,Y]),
    Y<4,
    Z is Y+1,
    not(member([X,Z], S)),
    append(S,[[X,Z]],S1),
    retractall(seguras(_)),
    assert(seguras(S1)).
casasegura90.

casasegura180:-
    seguras(S),
    posicao([X,Y]),
    X>1,
    Z is X-1,
    not(member([Z,Y], S)),
    append(S,[[Z,Y]],S1),
    retractall(seguras(_)),
    assert(seguras(S1)).
casasegura180.

casasegura270:-
    seguras(S),
    posicao([X,Y]),
    Y>1,
    Z is Y-1,
    not(member([X,Z], S)),
    append(S,[[X,Z]],S1),
    retractall(seguras(_)),
    assert(seguras(S1)).
casasegura270.

tiraperigosas:-
    seguras(S),
    perigosas(P),
    subtract(S,P, L),
    retractall(seguras(_)),
    assert(seguras(L)).
tiraperigosas.

reduz:-
    seguras(S),
    list_to_set(S,S1),
    retractall(seguras(_)),
    assert(seguras(S1)).
reduz.


%funcao para calcular casas que oferecem risco ao agente
verperigosas:-
    casasperigosas0,
    casasperigosas90,
    casasperigosas180,
    casasperigosas270,
    tirasegurasvisitadas.

casasperigosas0:-
    perigosas(P),
    posicao([X,Y]),
    X<4,
    Z is X+1,
    not(member([Z,Y], P)),
    append(P,[[Z,Y]],P1),
    retractall(perigosas(_)),
    assert(perigosas(P1)).
casasperigosas0.

casasperigosas90:-
    perigosas(P),
    posicao([X,Y]),
    Y<4,
    Z is Y+1,
    not(member([X,Z], P)),
    append(P,[[X,Z]],P1),
    retractall(perigosas(_)),
    assert(perigosas(P1)).
casasperigosas90.

casasperigosas180:-
    perigosas(P),
    posicao([X,Y]),
    X>1,
    Z is X-1,
    not(member([Z,Y], P)),
    append(P,[[Z,Y]],P1),
    retractall(perigosas(_)),
    assert(perigosas(P1)).
casasperigosas180.

casasperigosas270:-
    perigosas(P),
    posicao([X,Y]),
    Y>1,
    Z is Y-1,
    not(member([X,Z], P)),
    append(P,[[X,Z]],P1),
    retractall(perigosas(_)),
    assert(perigosas(P1)).
casasperigosas270.

tirasegurasvisitadas:-
    perigosas(P),
    seguras(S),
    visitadas(V),
    subtract(P,S, P1),
    subtract(P1,V, P2),
    retractall(perigosas(_)),
    assert(perigosas(P2)).
tirasegurasvisitadas.
    
decflecha:- %funcao para diminuir numero de flechas apos o tiro
    flecha(X0),
    X1 is X0-1,
    retractall(flecha(_)),
    assert(flecha(X1)).

target:- %funcao para definir uma casa-alvo ao agente002
    seguras([S|_]),
    retractall(alvo(_)),
    assert(alvo(S)).

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

