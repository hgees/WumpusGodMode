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
:-dynamic([flecha/1,direcao/1]).

wumpusworld(pit3, 4). %tipo, tamanho

init_agent:-
    retractall(flecha(_)),
    assert(flecha(1)),
    retractall(direcao(_)),
    assert(direcao(0)).

% esta funcao permanece a mesma. Nao altere.
restart_agent :- 
	init_agent.

% esta e a funcao chamada pelo simulador. Nao altere a "cabeca" da funcao. Apenas o corpo.
% Funcao recebe Percepcao, uma lista conforme descrito acima.
% Deve retornar uma Acao, dentre as acoes validas descritas acima.
run_agent(Percepcao, Acao) :-
  write('percebi: '), 
  writeln(Percepcao),
  coragem(Percepcao, Acao),
  direcao(Direcao),
  write('Direcao: '),
  writeln(Direcao).

%definindo direcao do agente.
direcao(0). %agente esta virado para direita.
% 0 -> direita, 90 -> cima, 180 -> esquerda, 270 -> baixo.

mudadir :-
    direcao(D0),
    D1 is D0 + 90,
    assert(direcao(D1)).


%inteligencia do agente
coragem([no,no,no,no,no], goforward). %vai pra frente se não sentir perigo 
coragem([_,_,no,yes,no], turnleft). %vira para a esquerda se trombar
coragem([_,yes,no,no,no],A).
coragem([_,_,yes,_,_], grab). %pega o ouro se sentir o brilho
coragem([yes,no,no,no,_], shoot) :-  %atira em linha reta se sentir fedor e tiver uma flecha
    flecha(1),
    decflecha, fail.
coragem([yes,_,_,no,_],gofoward).

decflecha :-
    flecha(X0),
    X1 is X0 -1,
    retractall(flecha(_)),
    assert(flecha(X1)).

