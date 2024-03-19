module Main where
   

import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Maybe
import GHC.Float
import LI12324 
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4

main :: IO ()
main = do
   putStrLn "Hello, PrimateKong!"
   estadoinicial <- get_images estadoteste
   playIO
      display1
      corfundo
      60
      estadoinicial
      whatToDraw
      handleEvent
      movimentate

display1 :: Display
display1 = FullScreen

data Menuescolher = Jogar | Opcoes | Sair | Menu deriving (Eq, Show)
-- Estrutura de dados para armazenar o estado do menu
data EstadoGloss = EstadoGloss { opcaoEscolhida :: Menuescolher,
                                   opcaoAtual     :: Menuescolher,
                                   jogo           :: Jogo,
                                   imagens        :: Imagens} deriving (Show)

{- | a funçao 'selecionar' verifica que opçao esta selecionada e muda-a de cor com a ajuda da
funçao 'opcoesParaSelecionar' que desenha o menu apresentando as possiveis opcoes

@
selecionar :: Picture -> Bool -> Picture
selecionar img estaSelecionado = pictures
 [ color (if estaSelecionado then blue else red) $ translate (-50) 50 img
 ]

@

@
opcoesParaSelecionar :: Picture -> Picture -> Picture -> Picture -> EstadoGloss -> Picture
opcoesParaSelecionar imgFundo imgJogar imgSair imgOpcoes estado = pictures
 [translate 0 0 (scaleImage Menu imgFundo),
   translate (-500) 0 (scaleImage Jogar imgJogar),
    translate 0 0 (scaleImage Opcoes imgOpcoes),
    translate 500 0 (scaleImage Sair imgSair)
 ]
 where
   scaleImage opcao img = scale (if opcaoEscolhida estado == opcao then 0.7 else 0.6) (if opcaoEscolhida estado == opcao then 0.7 else 0.6) (selecionar img (opcaoEscolhida estado == opcao))

@
-}
selecionar :: Picture -> Bool -> Picture
selecionar img estaSelecionado = pictures
 [ color (if estaSelecionado then blue else red) $ translate (-50) 50 img
 ]

opcoesParaSelecionar :: Picture -> Picture -> Picture -> Picture -> EstadoGloss -> Picture
opcoesParaSelecionar imgFundo imgJogar imgSair imgOpcoes estado = pictures
 [translate 0 0 (scaleImage Menu imgFundo),
   translate (-500) 0 (scaleImage Jogar imgJogar),
    translate 0 0 (scaleImage Opcoes imgOpcoes),
    translate 500 0 (scaleImage Sair imgSair)
 ]
 where
   scaleImage opcao img = scale (if opcaoEscolhida estado == opcao then 0.7 else 0.6) (if opcaoEscolhida estado == opcao then 0.7 else 0.6) (selecionar img (opcaoEscolhida estado == opcao))

{-| a funcao 'whatToDraw' dependendo da selacao desenha o novo estado

@
whatToDraw ::  EstadoGloss -> IO Picture
whatToDraw estado
 | opcaoAtual estado == Jogar = return $ jogarGloss1 estado 
 | opcaoAtual estado == Opcoes = return $ jogarGloss2 estado
 | opcaoAtual estado == Menu = return $ opcoesParaSelecionar texmenu texjogar texsaida texoptions estado
 | otherwise = return $ Color green $ rectangleSolid 10 10
   where texmenu = fromJust (lookup "fundo" (imagens estado))
         texjogar = fromJust (lookup "jogar" (imagens estado))
         texoptions = fromJust (lookup "opcoes" (imagens estado))
         texsaida = fromJust (lookup "saida" (imagens estado))

@
-}

whatToDraw ::  EstadoGloss -> IO Picture
whatToDraw estado
 | opcaoAtual estado == Jogar = return $ jogarGloss1 estado 
 | opcaoAtual estado == Opcoes = return $ jogarGloss2 estado
 | opcaoAtual estado == Menu = return $ opcoesParaSelecionar texmenu texjogar texsaida texoptions estado
 | otherwise = return $ Color green $ rectangleSolid 10 10
   where texmenu = fromJust (lookup "fundo" (imagens estado))
         texjogar = fromJust (lookup "jogar" (imagens estado))
         texoptions = fromJust (lookup "opcoes" (imagens estado))
         texsaida = fromJust (lookup "saida" (imagens estado))

{-| a funcao 'getPosicaoFinal' da a posicao do mapa

@
getPosicaoFinal :: Mapa -> Posicao
getPosicaoFinal (Mapa _ pf _) = pf

@
-}
getPosicaoFinal :: Mapa -> Posicao
getPosicaoFinal (Mapa _ pf _) = pf

{-| a funcao 'jogarGloss1' desenha o grafico do mario
a funcao 'gethitboxFantasmas' da a hitbox dos fantasmas
a funcao 'getHitboxPosicao' da a hitbox de uma posicao
a funcao 'jogarGloss2' desenha o grafico do futebol

@
jogarGloss1 :: EstadoGloss -> Picture
jogarGloss1 estado =  Pictures $ concat
  [ map (desenhar vazioImg) listaVazio
  , map (desenhar plataformaImg) listaPlataforma
  , map (desenharAlcapao alcapaoImg) listaAlcapao
  , map (desenhar escadaImg) listaEscada
  , map (desenharFogo fogoImg) listaFogo
  , map (desenhar fantasmaImg) listaFantasma
  , map (desenhaPassaro passaroImg) listaPassaro
  , map (desenhaKong kongImg) listaKong
  , map (desenharMartelo marteloImg) listaMartelo
  , map (desenhar moedaImg) listaMoeda
  , map (desenharAnimacao jogadorImg) listaJogadorJogo
  , map (desenhaVidas vidasimg) listaPosicaoVidas
  , map (desenharPontos scoreimg) listaScore
  , map (desenharPontos imgLevel) listaLevel
  , map (desenhar princesaimg) listaPrincesa
  , map (desenhar winIMg) listaWin
  ]
  where
    listaPlataforma = tiraposicaobloco Plataforma blocos
    listaAlcapao = tiraposicaobloco Alcapao blocos
    listaVazio = tiraposicaobloco Vazio blocos
    listaEscada = tiraposicaobloco Escada blocos
    listaFantasma = sacaFantasma (inimigos (jogo estado))
    listaKong = sacaKong (inimigos (jogo estado))
    listaJogadorJogo = [posicao (jogador (jogo estado))]
    listaMartelo = sacaMartelo (colecionaveis (jogo estado))
    listaMoeda = sacaMoeda (colecionaveis (jogo estado))
    listaFogo = tiraposicaobloco Fogo blocos
    listaPassaro = sacaPassaro (inimigos (jogo estado))
    listaPosicaoVidas = [(1.5,0.5)]
    listaScore = [(18.5,0.7)]
    listaPrincesa = [pf]
    listaWin = [(18.7,10.3)]
    listaLevel = [(35,3)]
    (Mapa (pos,_) pf blocos) = mapa (jogo estado)
    plataformaImg = fromJust (lookup "plataforma" (imagens estado))
    passaroImg = fromJust (lookup "passaro" (imagens estado))
    alcapaoImg = fromJust (lookup "alcapao" (imagens estado))
    vazioImg = fromJust (lookup "vazio" (imagens estado))
    fogoImg = fromJust (lookup "fogo" (imagens estado))
    escadaImg = fromJust (lookup "escada" (imagens estado))
    jogadorImg | direcao (jogador (jogo estado)) == Oeste && emEscada (jogador (jogo estado)) = fromJust (lookup "jogadorAnimadoEscada" (imagens estado))
               | direcao (jogador (jogo estado)) == Este && emEscada (jogador (jogo estado)) = fromJust (lookup "jogadorAnimadoEscada" (imagens estado))
               | not (estaemPlataforma (mapa (jogo estado)) (jogador (jogo estado))) && not (emEscada (jogador(jogo estado))) = fromJust (lookup "marioGravidade" (imagens estado))
               | fst (aplicaDano (jogador (jogo estado))) && direcao (jogador (jogo estado)) == Este && intersecaoTotalListas (getdamagehitbox (jogador (jogo estado))) (gethitboxFantasmas (inimigos (jogo estado))) = fromJust (lookup "jogadorAtacaEste" (imagens estado))
               | fst (aplicaDano (jogador (jogo estado))) && direcao (jogador (jogo estado)) == Oeste && intersecaoTotalListas (getdamagehitbox (jogador (jogo estado))) (gethitboxFantasmas (inimigos (jogo estado))) = fromJust (lookup "jogadorAtaca" (imagens estado))
               | fst (aplicaDano (jogador (jogo estado))) && direcao (jogador (jogo estado)) == Este = fromJust (lookup "jogadorArmadoEste" (imagens estado))
               | fst (aplicaDano (jogador (jogo estado))) && direcao (jogador (jogo estado)) == Oeste = fromJust (lookup "jogadorArmado" (imagens estado))
               | direcao (jogador (jogo estado)) == Este = fromJust (lookup "jogadorAnimadoEste" (imagens estado))
               | direcao (jogador (jogo estado)) == Oeste = fromJust (lookup "jogadorAnimado" (imagens estado))
               | otherwise = fromJust (lookup "jogadorAnimado" (imagens estado))
    fantasmaImg = fromJust (lookup "fantasma" (imagens estado))
    kongImg = fromJust (lookup "newKong" (imagens estado))
    marteloImg = fromJust (lookup "martelo" (imagens estado))
    moedaImg = fromJust (lookup "moeda" (imagens estado))
    imgLevel = fromJust (lookup "level1" (imagens estado))
    vidasimg = if vida (jogador (jogo estado)) == 3 then fromJust (lookup "vidas3" (imagens estado)) else menosvidas
    menosvidas = if vida (jogador (jogo estado)) == 2 then fromJust (lookup "vidas2" (imagens estado)) else menosvidas2
    menosvidas2 = if vida (jogador (jogo estado)) == 1 then fromJust (lookup "vidas1" (imagens estado)) else error "Ficaste sem vidas"
    scoreimg = if pontos (jogador (jogo estado)) == 0 then fromJust (lookup "score0" (imagens estado)) else score2
    score2 = if pontos (jogador (jogo estado)) == 10 then fromJust (lookup "score100" (imagens estado)) else score3
    score3 = if pontos (jogador (jogo estado)) == 20 then fromJust (lookup "score500" (imagens estado)) else score4
    score4 = if pontos (jogador (jogo estado)) == 30 then fromJust (lookup "score1000" (imagens estado)) else score5
    score5 = if pontos (jogador (jogo estado)) == 40 then fromJust (lookup "score1500" (imagens estado)) else score6
    score6 = if pontos (jogador (jogo estado)) == 50 then fromJust (lookup "score5000" (imagens estado)) else fromJust (lookup "score0" (imagens estado))
    princesaimg = fromJust (lookup "princesa" (imagens estado))
    winIMg = if intersecaoTotal (gethitbox (jogador (jogo estado))) (getHitboxPosicao (getPosicaoFinal (mapa (jogo estado)))) then fromJust (lookup "Win" (imagens estado)) else fromJust (lookup "vazio3" (imagens estado))

@

@
gethitboxFantasmas :: [Personagem] -> [Hitbox] 
gethitboxFantasmas inimigos = map gethitbox inimigos

@

@
getHitboxPosicao :: Posicao -> Hitbox
getHitboxPosicao (x,y) = ((x-0.5,y-0.5),(x+0.5,y+0.5))

@

@
jogarGloss2 :: EstadoGloss -> Picture
jogarGloss2 estado =  Pictures $ concat
  [ map (desenhar vazioImg) listaVazio
  , map (desenhar plataformaImg) listaPlataforma
  , map (desenharAlcapao alcapaoImg) listaAlcapao
  , map (desenhar escadaImg) listaEscada
  , map (desenharFogo fogoimg) listaFogo
  , map (desenhar fantasmaImg) listaFantasma
  , map (desenhar kongImg) listaKong
  , map (desenhaPassaro passaroImg) listaPassaro
  , map (desenhar marteloImg) listaMartelo
  , map (desenhar moedaImg) listaMoeda
  , map (desenhar jogadorImg) listaJogadorJogo
  , map (desenhar vidasimg) listaPosicaoVidas
  , map (desenharPontos scoreimg) listaScore
  , map (desenharPontos imgLevel) listaLevel
  , map (desenhar princesaimg2) listaPrincesa
  , map (desenhar winIMg) listaWin
  ]
  where
    listaPlataforma = tiraposicaobloco Plataforma blocos
    listaAlcapao = tiraposicaobloco Alcapao blocos
    listaVazio = tiraposicaobloco Vazio blocos
    listaEscada = tiraposicaobloco Escada blocos
    listaFantasma = sacaFantasma (inimigos (jogo estado))
    listaPassaro = sacaPassaro (inimigos (jogo estado))
    listaKong = sacaKong (inimigos (jogo estado))
    listaJogadorJogo = [posicao (jogador (jogo estado))]
    listaMartelo = sacaMartelo (colecionaveis (jogo estado))
    listaMoeda = sacaMoeda (colecionaveis (jogo estado))
    listaPosicaoVidas = [(2.5,1.5)]
    listaScore = [(19,0.5)]
    listaLevel = [(35,3)]
    listaPrincesa = [pf]
    listaWin = [(18.7,10.3)]
    listaFogo = tiraposicaobloco Fogo blocos
    (Mapa (pos,_) pf blocos) = mapa (jogo estado)
    plataformaImg = fromJust (lookup "plataforma3" (imagens estado))
    alcapaoImg = fromJust (lookup "alcapao" (imagens estado))
    vazioImg = fromJust (lookup "vazio2" (imagens estado))
    escadaImg = fromJust (lookup "escada2" (imagens estado))
    jogadorImg | direcao (jogador (jogo estado)) == Este = fromJust (lookup "jogador2" (imagens estado))
               | direcao (jogador (jogo estado)) == Oeste = fromJust (lookup "jogadorOeste2" (imagens estado))
               | otherwise = fromJust (lookup "jogador2" (imagens estado))
    fantasmaImg = fromJust (lookup "fantasma2" (imagens estado))
    kongImg = fromJust (lookup "anao" (imagens estado))
    marteloImg = fromJust (lookup "martelo" (imagens estado))
    moedaImg = fromJust (lookup "moeda" (imagens estado))
    vidasimg = if vida (jogador (jogo estado)) == 3 then fromJust (lookup "vidasRonaldo3" (imagens estado)) else menosvidas
    menosvidas = if vida (jogador (jogo estado)) == 2 then fromJust (lookup "vidasRonaldo2" (imagens estado)) else menosvidas2
    menosvidas2 = if vida (jogador (jogo estado)) == 1 then fromJust (lookup "vidasRonaldo1" (imagens estado)) else error "Ficaste sem vidas"
    scoreimg = if pontos (jogador (jogo estado)) == 0 then fromJust (lookup "score0" (imagens estado)) else score2
    score2 = if pontos (jogador (jogo estado)) == 10 then fromJust (lookup "score100" (imagens estado)) else score3
    score3 = if pontos (jogador (jogo estado)) == 20 then fromJust (lookup "score500" (imagens estado)) else score4
    score4 = if pontos (jogador (jogo estado)) == 30 then fromJust (lookup "score1000" (imagens estado)) else score5
    score5 = if pontos (jogador (jogo estado)) == 40 then fromJust (lookup "score1500" (imagens estado)) else score6
    score6 = if pontos (jogador (jogo estado)) == 50 then fromJust (lookup "score5000" (imagens estado)) else fromJust (lookup "score0" (imagens estado))
    princesaimg2 = fromJust (lookup "ballondor" (imagens estado))
    imgLevel = fromJust (lookup "level1" (imagens estado))
    fogoimg = fromJust (lookup "fogo" (imagens estado))
    passaroImg = fromJust (lookup "passaro" (imagens estado))
    winIMg = if intersecaoTotal (gethitbox (jogador (jogo estado))) (getHitboxPosicao (getPosicaoFinal (mapa (jogo estado)))) then fromJust (lookup "winner2" (imagens estado)) else fromJust (lookup "vazio3" (imagens estado))

@
-}
jogarGloss1 :: EstadoGloss -> Picture
jogarGloss1 estado =  Pictures $ concat
  [ map (desenhar vazioImg) listaVazio
  , map (desenhar plataformaImg) listaPlataforma
  , map (desenharAlcapao alcapaoImg) listaAlcapao
  , map (desenhar escadaImg) listaEscada
  , map (desenharFogo fogoImg) listaFogo
  , map (desenhar fantasmaImg) listaFantasma
  , map (desenhaPassaro passaroImg) listaPassaro
  , map (desenhaKong kongImg) listaKong
  , map (desenharMartelo marteloImg) listaMartelo
  , map (desenhar moedaImg) listaMoeda
  , map (desenharAnimacao jogadorImg) listaJogadorJogo
  , map (desenhaVidas vidasimg) listaPosicaoVidas
  , map (desenharPontos scoreimg) listaScore
  , map (desenharPontos imgLevel) listaLevel
  , map (desenhar princesaimg) listaPrincesa
  , map (desenhar winIMg) listaWin
  ]
  where
    listaPlataforma = tiraposicaobloco Plataforma blocos
    listaAlcapao = tiraposicaobloco Alcapao blocos
    listaVazio = tiraposicaobloco Vazio blocos
    listaEscada = tiraposicaobloco Escada blocos
    listaFantasma = sacaFantasma (inimigos (jogo estado))
    listaKong = sacaKong (inimigos (jogo estado))
    listaJogadorJogo = [posicao (jogador (jogo estado))]
    listaMartelo = sacaMartelo (colecionaveis (jogo estado))
    listaMoeda = sacaMoeda (colecionaveis (jogo estado))
    listaFogo = tiraposicaobloco Fogo blocos
    listaPassaro = sacaPassaro (inimigos (jogo estado))
    listaPosicaoVidas = [(1.5,0.5)]
    listaScore = [(18.5,0.7)]
    listaPrincesa = [pf]
    listaWin = [(18.7,10.3)]
    listaLevel = [(35,3)]
    (Mapa (pos,_) pf blocos) = mapa (jogo estado)
    plataformaImg = fromJust (lookup "plataforma" (imagens estado))
    passaroImg = fromJust (lookup "passaro" (imagens estado))
    alcapaoImg = fromJust (lookup "alcapao" (imagens estado))
    vazioImg = fromJust (lookup "vazio" (imagens estado))
    fogoImg = fromJust (lookup "fogo" (imagens estado))
    escadaImg = fromJust (lookup "escada" (imagens estado))
    jogadorImg | direcao (jogador (jogo estado)) == Oeste && emEscada (jogador (jogo estado)) = fromJust (lookup "jogadorAnimadoEscada" (imagens estado))
               | direcao (jogador (jogo estado)) == Este && emEscada (jogador (jogo estado)) = fromJust (lookup "jogadorAnimadoEscada" (imagens estado))
               | not (estaemPlataforma (mapa (jogo estado)) (jogador (jogo estado))) && not (emEscada (jogador(jogo estado))) = fromJust (lookup "marioGravidade" (imagens estado))
               | fst (aplicaDano (jogador (jogo estado))) && direcao (jogador (jogo estado)) == Este && intersecaoTotalListas (getdamagehitbox (jogador (jogo estado))) (gethitboxFantasmas (inimigos (jogo estado))) = fromJust (lookup "jogadorAtacaEste" (imagens estado))
               | fst (aplicaDano (jogador (jogo estado))) && direcao (jogador (jogo estado)) == Oeste && intersecaoTotalListas (getdamagehitbox (jogador (jogo estado))) (gethitboxFantasmas (inimigos (jogo estado))) = fromJust (lookup "jogadorAtaca" (imagens estado))
               | fst (aplicaDano (jogador (jogo estado))) && direcao (jogador (jogo estado)) == Este = fromJust (lookup "jogadorArmadoEste" (imagens estado))
               | fst (aplicaDano (jogador (jogo estado))) && direcao (jogador (jogo estado)) == Oeste = fromJust (lookup "jogadorArmado" (imagens estado))
               | direcao (jogador (jogo estado)) == Este = fromJust (lookup "jogadorAnimadoEste" (imagens estado))
               | direcao (jogador (jogo estado)) == Oeste = fromJust (lookup "jogadorAnimado" (imagens estado))
               | otherwise = fromJust (lookup "jogadorAnimado" (imagens estado))
    fantasmaImg = fromJust (lookup "fantasma" (imagens estado))
    kongImg = fromJust (lookup "newKong" (imagens estado))
    marteloImg = fromJust (lookup "martelo" (imagens estado))
    moedaImg = fromJust (lookup "moeda" (imagens estado))
    imgLevel = fromJust (lookup "level1" (imagens estado))
    vidasimg = if vida (jogador (jogo estado)) == 3 then fromJust (lookup "vidas3" (imagens estado)) else menosvidas
    menosvidas = if vida (jogador (jogo estado)) == 2 then fromJust (lookup "vidas2" (imagens estado)) else menosvidas2
    menosvidas2 = if vida (jogador (jogo estado)) == 1 then fromJust (lookup "vidas1" (imagens estado)) else error "Ficaste sem vidas"
    scoreimg = if pontos (jogador (jogo estado)) == 0 then fromJust (lookup "score0" (imagens estado)) else score2
    score2 = if pontos (jogador (jogo estado)) == 10 then fromJust (lookup "score100" (imagens estado)) else score3
    score3 = if pontos (jogador (jogo estado)) == 20 then fromJust (lookup "score500" (imagens estado)) else score4
    score4 = if pontos (jogador (jogo estado)) == 30 then fromJust (lookup "score1000" (imagens estado)) else score5
    score5 = if pontos (jogador (jogo estado)) == 40 then fromJust (lookup "score1500" (imagens estado)) else score6
    score6 = if pontos (jogador (jogo estado)) == 50 then fromJust (lookup "score5000" (imagens estado)) else fromJust (lookup "score0" (imagens estado))
    princesaimg = fromJust (lookup "princesa" (imagens estado))
    winIMg = if intersecaoTotal (gethitbox (jogador (jogo estado))) (getHitboxPosicao (getPosicaoFinal (mapa (jogo estado)))) then fromJust (lookup "Win" (imagens estado)) else fromJust (lookup "vazio3" (imagens estado))

gethitboxFantasmas :: [Personagem] -> [Hitbox] 
gethitboxFantasmas inimigos = map gethitbox inimigos

getHitboxPosicao :: Posicao -> Hitbox
getHitboxPosicao (x,y) = ((x-0.5,y-0.5),(x+0.5,y+0.5))

jogarGloss2 :: EstadoGloss -> Picture
jogarGloss2 estado =  Pictures $ concat
  [ map (desenhar vazioImg) listaVazio
  , map (desenhar plataformaImg) listaPlataforma
  , map (desenharAlcapao alcapaoImg) listaAlcapao
  , map (desenhar escadaImg) listaEscada
  , map (desenharFogo fogoimg) listaFogo
  , map (desenhar fantasmaImg) listaFantasma
  , map (desenhar kongImg) listaKong
  , map (desenhaPassaro passaroImg) listaPassaro
  , map (desenhar marteloImg) listaMartelo
  , map (desenhar moedaImg) listaMoeda
  , map (desenhar jogadorImg) listaJogadorJogo
  , map (desenhar vidasimg) listaPosicaoVidas
  , map (desenharPontos scoreimg) listaScore
  , map (desenharPontos imgLevel) listaLevel
  , map (desenhar princesaimg2) listaPrincesa
  , map (desenhar winIMg) listaWin
  ]
  where
    listaPlataforma = tiraposicaobloco Plataforma blocos
    listaAlcapao = tiraposicaobloco Alcapao blocos
    listaVazio = tiraposicaobloco Vazio blocos
    listaEscada = tiraposicaobloco Escada blocos
    listaFantasma = sacaFantasma (inimigos (jogo estado))
    listaPassaro = sacaPassaro (inimigos (jogo estado))
    listaKong = sacaKong (inimigos (jogo estado))
    listaJogadorJogo = [posicao (jogador (jogo estado))]
    listaMartelo = sacaMartelo (colecionaveis (jogo estado))
    listaMoeda = sacaMoeda (colecionaveis (jogo estado))
    listaPosicaoVidas = [(2.5,1.5)]
    listaScore = [(19,0.5)]
    listaLevel = [(35,3)]
    listaPrincesa = [pf]
    listaWin = [(18.7,10.3)]
    listaFogo = tiraposicaobloco Fogo blocos
    (Mapa (pos,_) pf blocos) = mapa (jogo estado)
    plataformaImg = fromJust (lookup "plataforma3" (imagens estado))
    alcapaoImg = fromJust (lookup "alcapao" (imagens estado))
    vazioImg = fromJust (lookup "vazio2" (imagens estado))
    escadaImg = fromJust (lookup "escada2" (imagens estado))
    jogadorImg | direcao (jogador (jogo estado)) == Este = fromJust (lookup "jogador2" (imagens estado))
               | direcao (jogador (jogo estado)) == Oeste = fromJust (lookup "jogadorOeste2" (imagens estado))
               | otherwise = fromJust (lookup "jogador2" (imagens estado))
    fantasmaImg = fromJust (lookup "fantasma2" (imagens estado))
    kongImg = fromJust (lookup "anao" (imagens estado))
    marteloImg = fromJust (lookup "martelo" (imagens estado))
    moedaImg = fromJust (lookup "moeda" (imagens estado))
    vidasimg = if vida (jogador (jogo estado)) == 3 then fromJust (lookup "vidasRonaldo3" (imagens estado)) else menosvidas
    menosvidas = if vida (jogador (jogo estado)) == 2 then fromJust (lookup "vidasRonaldo2" (imagens estado)) else menosvidas2
    menosvidas2 = if vida (jogador (jogo estado)) == 1 then fromJust (lookup "vidasRonaldo1" (imagens estado)) else error "Ficaste sem vidas"
    scoreimg = if pontos (jogador (jogo estado)) == 0 then fromJust (lookup "score0" (imagens estado)) else score2
    score2 = if pontos (jogador (jogo estado)) == 10 then fromJust (lookup "score100" (imagens estado)) else score3
    score3 = if pontos (jogador (jogo estado)) == 20 then fromJust (lookup "score500" (imagens estado)) else score4
    score4 = if pontos (jogador (jogo estado)) == 30 then fromJust (lookup "score1000" (imagens estado)) else score5
    score5 = if pontos (jogador (jogo estado)) == 40 then fromJust (lookup "score1500" (imagens estado)) else score6
    score6 = if pontos (jogador (jogo estado)) == 50 then fromJust (lookup "score5000" (imagens estado)) else fromJust (lookup "score0" (imagens estado))
    princesaimg2 = fromJust (lookup "ballondor" (imagens estado))
    imgLevel = fromJust (lookup "level1" (imagens estado))
    fogoimg = fromJust (lookup "fogo" (imagens estado))
    passaroImg = fromJust (lookup "passaro" (imagens estado))
    winIMg = if intersecaoTotal (gethitbox (jogador (jogo estado))) (getHitboxPosicao (getPosicaoFinal (mapa (jogo estado)))) then fromJust (lookup "winner2" (imagens estado)) else fromJust (lookup "vazio3" (imagens estado))

{-| as funçoes 'desenhar' 'desenharAnimacao' 'desenhaKong' 'desenhaVidas' 'desenharPontos' 'desenharFogo' 'desenharMartelo' 'desenharAlcapao' 'desenhaPassaro' servem
para desenhar todos os elementos do jogo

@
desenhar :: Picture -> Posicao -> Picture
desenhar img (a, b) = translate ((double2Float a *  escaladogloss) - 933.5) ((double2Float (negate b) *  escaladogloss) + 513.5) $ scale 0.5 0.5 img

@

@
desenharAnimacao :: Picture -> Posicao -> Picture
desenharAnimacao img (a, b) = translate ((double2Float a *  escaladogloss) - 933.5) ((double2Float (negate b) *  escaladogloss) + 513.5) $ scale 2.0 2.7 img

@

@
desenhaKong :: Picture -> Posicao -> Picture
desenhaKong img (a, b) = translate ((double2Float a *  escaladogloss) - 933.5) ((double2Float (negate b) *  escaladogloss) + 513.5) $ scale 4 4.3 img

@

@
desenhaVidas :: Picture -> Posicao -> Picture
desenhaVidas img (a, b) = translate ((double2Float a *  escaladogloss) - 933.5) ((double2Float (negate b) *  escaladogloss) + 513.5) $ scale 0.2 0.2 img

@

@
desenharPontos :: Picture -> Posicao -> Picture
desenharPontos img (a, b) = translate ((double2Float a *  escaladogloss) - 933.5) ((double2Float (negate b) *  escaladogloss) + 513.5) $ scale 4.0 4.0 img

@

@
desenharFogo :: Picture -> Posicao -> Picture
desenharFogo img (a, b) = translate ((double2Float a *  escaladogloss) - 933.5) ((double2Float (negate b) *  escaladogloss) + 513.5) $ scale 3.7 3.6 img

@

@
desenharMartelo :: Picture -> Posicao -> Picture
desenharMartelo img (a, b) = translate ((double2Float a *  escaladogloss) - 933.5) ((double2Float (negate b) *  escaladogloss) + 513.5) $ scale 3.7 3.6 img

@

@
desenharAlcapao :: Picture -> Posicao -> Picture
desenharAlcapao img (a, b) = translate ((double2Float a *  escaladogloss) - 933.5) ((double2Float (negate b) *  escaladogloss) + 513.5) $ scale 6 6 img

@

@
desenhaPassaro :: Picture -> Posicao -> Picture
desenhaPassaro img (a, b) = translate ((double2Float a *  escaladogloss) - 933.5) ((double2Float (negate b) *  escaladogloss) + 513.5) $ scale 3 3 img

@
-}
escaladogloss :: Float
escaladogloss = 50

desenhar :: Picture -> Posicao -> Picture
desenhar img (a, b) = translate ((double2Float a *  escaladogloss) - 933.5) ((double2Float (negate b) *  escaladogloss) + 513.5) $ scale 0.5 0.5 img

desenharAnimacao :: Picture -> Posicao -> Picture
desenharAnimacao img (a, b) = translate ((double2Float a *  escaladogloss) - 933.5) ((double2Float (negate b) *  escaladogloss) + 513.5) $ scale 2.0 2.7 img

desenhaKong :: Picture -> Posicao -> Picture
desenhaKong img (a, b) = translate ((double2Float a *  escaladogloss) - 933.5) ((double2Float (negate b) *  escaladogloss) + 513.5) $ scale 4 4.3 img

desenhaVidas :: Picture -> Posicao -> Picture
desenhaVidas img (a, b) = translate ((double2Float a *  escaladogloss) - 933.5) ((double2Float (negate b) *  escaladogloss) + 513.5) $ scale 0.2 0.2 img

desenharPontos :: Picture -> Posicao -> Picture
desenharPontos img (a, b) = translate ((double2Float a *  escaladogloss) - 933.5) ((double2Float (negate b) *  escaladogloss) + 513.5) $ scale 4.0 4.0 img

desenharFogo :: Picture -> Posicao -> Picture
desenharFogo img (a, b) = translate ((double2Float a *  escaladogloss) - 933.5) ((double2Float (negate b) *  escaladogloss) + 513.5) $ scale 3.7 3.6 img

desenharMartelo :: Picture -> Posicao -> Picture
desenharMartelo img (a, b) = translate ((double2Float a *  escaladogloss) - 933.5) ((double2Float (negate b) *  escaladogloss) + 513.5) $ scale 3.7 3.6 img

desenharAlcapao :: Picture -> Posicao -> Picture
desenharAlcapao img (a, b) = translate ((double2Float a *  escaladogloss) - 933.5) ((double2Float (negate b) *  escaladogloss) + 513.5) $ scale 6 6 img

desenhaPassaro :: Picture -> Posicao -> Picture
desenhaPassaro img (a, b) = translate ((double2Float a *  escaladogloss) - 933.5) ((double2Float (negate b) *  escaladogloss) + 513.5) $ scale 3 3 img

{-| a funcao 'corfundo' escolhe a cor de fundo para depois utilizar na função main
a funcao ' estadoteste e o estado inicial do jogo

@
corfundo :: Color
corfundo = makeColorI 50 50 50 255

@

@
estadoteste :: EstadoGloss 
estadoteste = EstadoGloss {opcaoEscolhida = Menu, opcaoAtual = Menu, jogo = joguinho}

@
-}
corfundo :: Color
corfundo = makeColorI 50 50 50 255


estadoteste :: EstadoGloss 
estadoteste = EstadoGloss {opcaoEscolhida = Menu, opcaoAtual = Menu, jogo = joguinho}


{-| as funcoes 'handleEvent' e 'eventosJogo' servem para movimentar o joogo com eventos do teclado

@
handleEvent :: Event -> EstadoGloss -> IO EstadoGloss
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) estado = if opcaoAtual estado == Jogar then return $ estado {opcaoAtual = Menu} else return $ estado {opcaoAtual = Jogar}
handleEvent e estado
   | opcaoAtual estado == Menu = return $ handleMenuKeys e estado
   | otherwise = return $ estado { jogo = eventosJogo e (jogo estado) }

@

@
handleMenuKeys :: Event -> EstadoGloss -> EstadoGloss
handleMenuKeys (EventKey (SpecialKey KeyRight) Down _ _) estado =
   estado { opcaoEscolhida = opcaoAnterior (opcaoEscolhida estado) }
handleMenuKeys (EventKey (SpecialKey KeyLeft) Down _ _) estado =
   estado { opcaoEscolhida = proximaOpcao (opcaoEscolhida estado) }
handleMenuKeys (EventKey (SpecialKey KeyEnter) Down _ _) estado    | opcaoEscolhida estado == Sair = error "O jogo foi encerrado"
   | opcaoEscolhida estado == Jogar =  estado {opcaoAtual = Jogar} 
   | opcaoEscolhida estado == Opcoes = estado {opcaoAtual = Opcoes}
   | otherwise = estado
handleMenuKeys _ estado = estado

@

@
eventosJogo :: Event ->  Jogo -> Jogo
eventosJogo (EventKey (SpecialKey KeyRight) Down _ _) jogo = atualiza [Nothing, Nothing, Nothing, Nothing] (Just AndarDireita) jogo 
eventosJogo (EventKey (SpecialKey KeyRight) Up _ _) jogo = atualiza [Nothing, Nothing, Nothing, Nothing] (Just Parar) jogo
eventosJogo (EventKey (SpecialKey KeyLeft) Down _ _) jogo = atualiza [Nothing, Nothing, Nothing, Nothing] (Just AndarEsquerda) jogo
eventosJogo (EventKey (SpecialKey KeyLeft) Up _ _) jogo = atualiza [Nothing, Nothing, Nothing, Nothing] (Just Parar) jogo
eventosJogo (EventKey (SpecialKey KeyUp) Down _ _) jogo = atualiza [Nothing, Nothing, Nothing, Nothing] (Just Subir) jogo
eventosJogo (EventKey (SpecialKey KeyUp) Up _ _) jogo = atualiza [Nothing, Nothing, Nothing, Nothing] (Just Parar) jogo
eventosJogo (EventKey (SpecialKey KeyDown) Down _ _) jogo = atualiza [Nothing, Nothing, Nothing, Nothing] (Just Descer) jogo
eventosJogo (EventKey (SpecialKey KeyDown) Up _ _) jogo = atualiza [Nothing, Nothing, Nothing, Nothing] (Just Parar) jogo
eventosJogo (EventKey (SpecialKey KeySpace) Down _ _) jogo = atualiza [Nothing, Nothing, Nothing, Nothing] (Just Saltar) jogo
eventosJogo _ jogo = jogo

@
-}

handleEvent :: Event -> EstadoGloss -> IO EstadoGloss
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) estado = if opcaoAtual estado == Jogar then return $ estado {opcaoAtual = Menu} else return $ estado {opcaoAtual = Jogar}
handleEvent e estado
   | opcaoAtual estado == Menu = return $ handleMenuKeys e estado
   | otherwise = return $ estado { jogo = eventosJogo e (jogo estado) }

handleMenuKeys :: Event -> EstadoGloss -> EstadoGloss
handleMenuKeys (EventKey (SpecialKey KeyRight) Down _ _) estado =
   estado { opcaoEscolhida = opcaoAnterior (opcaoEscolhida estado) }
handleMenuKeys (EventKey (SpecialKey KeyLeft) Down _ _) estado =
   estado { opcaoEscolhida = proximaOpcao (opcaoEscolhida estado) }
handleMenuKeys (EventKey (SpecialKey KeyEnter) Down _ _) estado    | opcaoEscolhida estado == Sair = error "O jogo foi encerrado"
   | opcaoEscolhida estado == Jogar =  estado {opcaoAtual = Jogar} 
   | opcaoEscolhida estado == Opcoes = estado {opcaoAtual = Opcoes}
   | otherwise = estado
handleMenuKeys _ estado = estado

eventosJogo :: Event ->  Jogo -> Jogo
eventosJogo (EventKey (SpecialKey KeyRight) Down _ _) jogo = atualiza [Nothing, Nothing, Nothing, Nothing] (Just AndarDireita) jogo 
eventosJogo (EventKey (SpecialKey KeyRight) Up _ _) jogo = atualiza [Nothing, Nothing, Nothing, Nothing] (Just Parar) jogo
eventosJogo (EventKey (SpecialKey KeyLeft) Down _ _) jogo = atualiza [Nothing, Nothing, Nothing, Nothing] (Just AndarEsquerda) jogo
eventosJogo (EventKey (SpecialKey KeyLeft) Up _ _) jogo = atualiza [Nothing, Nothing, Nothing, Nothing] (Just Parar) jogo
eventosJogo (EventKey (SpecialKey KeyUp) Down _ _) jogo = atualiza [Nothing, Nothing, Nothing, Nothing] (Just Subir) jogo
eventosJogo (EventKey (SpecialKey KeyUp) Up _ _) jogo = atualiza [Nothing, Nothing, Nothing, Nothing] (Just Parar) jogo
eventosJogo (EventKey (SpecialKey KeyDown) Down _ _) jogo = atualiza [Nothing, Nothing, Nothing, Nothing] (Just Descer) jogo
eventosJogo (EventKey (SpecialKey KeyDown) Up _ _) jogo = atualiza [Nothing, Nothing, Nothing, Nothing] (Just Parar) jogo
eventosJogo (EventKey (SpecialKey KeySpace) Down _ _) jogo = atualiza [Nothing, Nothing, Nothing, Nothing] (Just Saltar) jogo
eventosJogo _ jogo = jogo

time :: Float
time = double2Float 1 / 60

tempinho :: Float 
tempinho = double2Float 1 / 60

{-| a funcao 'movimentate' coloca a funcao 'movimenta' da tarefa 3 no jogo

@
movimentate :: Float  -> EstadoGloss -> IO EstadoGloss
movimentate tempinho estado = do
  let novoJogo = movimenta semente (realToFrac tempinho) (jogo estado)
  return estado { jogo = novoJogo } 
  where tempinho = double2Float 1 / 60

@
-}

movimentate :: Float  -> EstadoGloss -> IO EstadoGloss
movimentate tempinho estado = do
  let novoJogo = movimenta semente (realToFrac tempinho) (jogo estado)
  return estado { jogo = novoJogo } 
  where tempinho = double2Float 1 / 60

semente :: Int 
semente = 5376555645

{-| a função 'proximaOpcao' serve para obter a próxima opção do menu
a função 'opcaoAnterior' serve para obter a opção anterior do menu

@
proximaOpcao :: Menuescolher -> Menuescolher
proximaOpcao Jogar = Sair
proximaOpcao Sair = Opcoes
proximaOpcao Opcoes = Jogar

@

@
opcaoAnterior :: Menuescolher -> Menuescolher
opcaoAnterior Jogar = Opcoes
opcaoAnterior Sair = Jogar
opcaoAnterior Opcoes = Sair
opcaoAnterior Menu = Jogar

@
-}

proximaOpcao :: Menuescolher -> Menuescolher
proximaOpcao Jogar = Sair
proximaOpcao Sair = Opcoes
proximaOpcao Opcoes = Jogar

opcaoAnterior :: Menuescolher -> Menuescolher
opcaoAnterior Jogar = Opcoes
opcaoAnterior Sair = Jogar
opcaoAnterior Opcoes = Sair
opcaoAnterior Menu = Jogar

{-| a funcao 'get_images' carrega as imagens

@
get_images :: EstadoGloss -> IO EstadoGloss -- carregar as imagens
get_images estado = do
      jogadorOeste <- loadBMP "imagensbmp1/jogadorOeste.bmp"
      moeda <- loadBMP "imagensbmp1/Moeda.bmp"
      martelo <- loadBMP "imagensbmp1/hammer.bmp"
      escada <- loadBMP "imagensbmp1/ladder.bmp"
      plataforma <- loadBMP "imagensbmp1/PlataformaMario.bmp"
      alcapao <- loadBMP "imagensbmp1/tube.bmp"
      vazio <- loadBMP "imagensbmp1/empty.bmp"
      vazio3 <- loadBMP "imagensbmp1/empty3.bmp"
      fundo <- loadBMP "imagensbmp1/fundinho2.bmp"
      fantasma <- loadBMP "imagensbmp1/ghost.bmp"
      passaro <- loadBMP "imagensbmp1/bird.bmp"
      macacoMalvado <- loadBMP "imagensbmp1/MacacoNovo.bmp"
      jogar <- loadBMP "imagensbmp1/playzinho.bmp"
      saida <- loadBMP "imagensbmp1/exit.bmp"
      opcoes <- loadBMP "imagensbmp1/PLay2.bmp"
      jogador2 <- loadBMP "imagensbmp1/Ronny1.bmp"
      jogadorOeste2 <- loadBMP "imagensbmp1/Ronny1Invert.bmp"
      vazio2 <- loadBMP "imagensbmp1/empty2.bmp"
      escada2 <- loadBMP "imagensbmp1/ladder2.bmp"
      fantasma2 <- loadBMP "imagensbmp1/ref.bmp"
      moeda2 <- loadBMP "imagensbmp1/goldenball.bmp"
      vidas3 <- loadBMP "imagensbmp1/3life.bmp"
      vidas2 <- loadBMP "imagensbmp1/2life.bmp"
      vidas1 <- loadBMP "imagensbmp1/1life.bmp"
      vidasRonaldo3 <- loadBMP "imagensbmp1/vidasRonaldo3.bmp"
      vidasRonaldo2 <- loadBMP "imagensbmp1/vidasRonaldo2.bmp"
      vidasRonaldo1 <- loadBMP "imagensbmp1/vidasRonaldo1.bmp"
      princesa <- loadBMP "imagensbmp1/Princesinha.bmp"
      win <- loadBMP "imagensbmp1/winner.bmp"
      winner2 <- loadBMP "imagensbmp1/winner2.bmp"
      jogadorAnimado <- loadBMP "imagensbmp1/marioAnimado.bmp"
      jogadorAnimadoEste <- loadBMP "imagensbmp1/marioAnimadoOeste.bmp"
      jogadorAnimadoEscada <- loadBMP "imagensbmp1/marioEscada.bmp"
      marioGravidade <- loadBMP "imagensbmp1/marioGravidade.bmp"
      marioArmado <- loadBMP "imagensbmp1/marioArmado.bmp"
      marioArmadoEste <- loadBMP "imagensbmp1/marioArmadoOeste2.bmp"
      marioAtaca <- loadBMP "imagensbmp1/marioAtaca.bmp"
      marioAtacaEste <- loadBMP "imagensbmp1/marioAtacaOeste.bmp"
      newKong <- loadBMP "imagensbmp1/MacacoNovo.bmp"
      ballondor <- loadBMP "imagensbmp1/ballondor.bmp"
      plataforma3 <- loadBMP "imagensbmp1/relvado.bmp"
      score0 <- loadBMP "imagensbmp1/score0.bmp"
      score100 <- loadBMP "imagensbmp1/score1.bmp"
      score500 <- loadBMP "imagensbmp1/score500.bmp"
      score1000 <- loadBMP "imagensbmp1/score1000.bmp"
      score1500 <- loadBMP "imagensbmp1/score1500.bmp"
      score5000 <- loadBMP "imagensbmp1/score5000.bmp"
      level1 <- loadBMP "imagensbmp1/level.bmp"
      fogo <- loadBMP "imagensbmp1/fire.bmp"
      anao <- loadBMP "imagensbmp1/anao.bmp"
      return estado { imagens = 
        [
         ("jogadorOeste",jogadorOeste),
         ("moeda",moeda),
         ("martelo",martelo),
         ("escada",escada),
         ("plataforma",plataforma),
         ("alcapao",alcapao),
         ("vazio",vazio),
         ("fundo",fundo),
         ("fantasma",fantasma),
         ("macacoMalvado",macacoMalvado),
         ("jogar",jogar),
         ("saida",saida),
         ("opcoes",opcoes),
         ("jogador2",jogador2),
         ("vazio2",vazio2),
         ("escada2",escada2),
         ("jogadorOeste2",jogadorOeste2),
         ("fantasma2",fantasma2),
         ("moeda2",moeda2),
         ("vidas3",vidas3),
         ("vidas2",vidas2),
         ("vidas1",vidas1),
         ("princesa",princesa),
         ("Win",win),
         ("jogadorAnimado",jogadorAnimado),
         ("jogadorAnimadoEste",jogadorAnimadoEste),
         ("jogadorAnimadoEscada",jogadorAnimadoEscada),
         ("marioGravidade",marioGravidade),
         ("jogadorArmado",marioArmado),
         ("jogadorArmadoEste",marioArmadoEste),
         ("jogadorAtaca",marioAtaca),
         ("jogadorAtacaEste",marioAtacaEste),
         ("vazio3",vazio3),
         ("newKong",newKong),
         ("vidasRonaldo3",vidasRonaldo3),
         ("vidasRonaldo2",vidasRonaldo2),
         ("vidasRonaldo1",vidasRonaldo1),
         ("winner2",winner2),
         ("ballondor",ballondor),
         ("plataforma3",plataforma3),
         ("score0",score0),
         ("score100",score100),
         ("score500",score500),
         ("score1000",score1000),
         ("score1500",score1500),
         ("score5000",score5000),
         ("level1",level1),
         ("fogo",fogo),
         ("passaro",passaro),
         ("anao",anao)
         ]}

@
-}
type Imagens = [(String,Picture)]

get_images :: EstadoGloss -> IO EstadoGloss -- carregar as imagens
get_images estado = do
      jogadorOeste <- loadBMP "imagensbmp1/jogadorOeste.bmp"
      moeda <- loadBMP "imagensbmp1/Moeda.bmp"
      martelo <- loadBMP "imagensbmp1/hammer.bmp"
      escada <- loadBMP "imagensbmp1/ladder.bmp"
      plataforma <- loadBMP "imagensbmp1/PlataformaMario.bmp"
      alcapao <- loadBMP "imagensbmp1/tube.bmp"
      vazio <- loadBMP "imagensbmp1/empty.bmp"
      vazio3 <- loadBMP "imagensbmp1/empty3.bmp"
      fundo <- loadBMP "imagensbmp1/fundinho2.bmp"
      fantasma <- loadBMP "imagensbmp1/ghost.bmp"
      passaro <- loadBMP "imagensbmp1/bird.bmp"
      macacoMalvado <- loadBMP "imagensbmp1/MacacoNovo.bmp"
      jogar <- loadBMP "imagensbmp1/playzinho.bmp"
      saida <- loadBMP "imagensbmp1/exit.bmp"
      opcoes <- loadBMP "imagensbmp1/PLay2.bmp"
      jogador2 <- loadBMP "imagensbmp1/Ronny1.bmp"
      jogadorOeste2 <- loadBMP "imagensbmp1/Ronny1Invert.bmp"
      vazio2 <- loadBMP "imagensbmp1/empty2.bmp"
      escada2 <- loadBMP "imagensbmp1/ladder2.bmp"
      fantasma2 <- loadBMP "imagensbmp1/ref.bmp"
      moeda2 <- loadBMP "imagensbmp1/goldenball.bmp"
      vidas3 <- loadBMP "imagensbmp1/3life.bmp"
      vidas2 <- loadBMP "imagensbmp1/2life.bmp"
      vidas1 <- loadBMP "imagensbmp1/1life.bmp"
      vidasRonaldo3 <- loadBMP "imagensbmp1/vidasRonaldo3.bmp"
      vidasRonaldo2 <- loadBMP "imagensbmp1/vidasRonaldo2.bmp"
      vidasRonaldo1 <- loadBMP "imagensbmp1/vidasRonaldo1.bmp"
      princesa <- loadBMP "imagensbmp1/Princesinha.bmp"
      win <- loadBMP "imagensbmp1/winner.bmp"
      winner2 <- loadBMP "imagensbmp1/winner2.bmp"
      jogadorAnimado <- loadBMP "imagensbmp1/marioAnimado.bmp"
      jogadorAnimadoEste <- loadBMP "imagensbmp1/marioAnimadoOeste.bmp"
      jogadorAnimadoEscada <- loadBMP "imagensbmp1/marioEscada.bmp"
      marioGravidade <- loadBMP "imagensbmp1/marioGravidade.bmp"
      marioArmado <- loadBMP "imagensbmp1/marioArmado.bmp"
      marioArmadoEste <- loadBMP "imagensbmp1/marioArmadoOeste2.bmp"
      marioAtaca <- loadBMP "imagensbmp1/marioAtaca.bmp"
      marioAtacaEste <- loadBMP "imagensbmp1/marioAtacaOeste.bmp"
      newKong <- loadBMP "imagensbmp1/MacacoNovo.bmp"
      ballondor <- loadBMP "imagensbmp1/ballondor.bmp"
      plataforma3 <- loadBMP "imagensbmp1/relvado.bmp"
      score0 <- loadBMP "imagensbmp1/score0.bmp"
      score100 <- loadBMP "imagensbmp1/score1.bmp"
      score500 <- loadBMP "imagensbmp1/score500.bmp"
      score1000 <- loadBMP "imagensbmp1/score1000.bmp"
      score1500 <- loadBMP "imagensbmp1/score1500.bmp"
      score5000 <- loadBMP "imagensbmp1/score5000.bmp"
      level1 <- loadBMP "imagensbmp1/level.bmp"
      fogo <- loadBMP "imagensbmp1/fire.bmp"
      anao <- loadBMP "imagensbmp1/anao.bmp"
      return estado { imagens = 
        [
         ("jogadorOeste",jogadorOeste),
         ("moeda",moeda),
         ("martelo",martelo),
         ("escada",escada),
         ("plataforma",plataforma),
         ("alcapao",alcapao),
         ("vazio",vazio),
         ("fundo",fundo),
         ("fantasma",fantasma),
         ("macacoMalvado",macacoMalvado),
         ("jogar",jogar),
         ("saida",saida),
         ("opcoes",opcoes),
         ("jogador2",jogador2),
         ("vazio2",vazio2),
         ("escada2",escada2),
         ("jogadorOeste2",jogadorOeste2),
         ("fantasma2",fantasma2),
         ("moeda2",moeda2),
         ("vidas3",vidas3),
         ("vidas2",vidas2),
         ("vidas1",vidas1),
         ("princesa",princesa),
         ("Win",win),
         ("jogadorAnimado",jogadorAnimado),
         ("jogadorAnimadoEste",jogadorAnimadoEste),
         ("jogadorAnimadoEscada",jogadorAnimadoEscada),
         ("marioGravidade",marioGravidade),
         ("jogadorArmado",marioArmado),
         ("jogadorArmadoEste",marioArmadoEste),
         ("jogadorAtaca",marioAtaca),
         ("jogadorAtacaEste",marioAtacaEste),
         ("vazio3",vazio3),
         ("newKong",newKong),
         ("vidasRonaldo3",vidasRonaldo3),
         ("vidasRonaldo2",vidasRonaldo2),
         ("vidasRonaldo1",vidasRonaldo1),
         ("winner2",winner2),
         ("ballondor",ballondor),
         ("plataforma3",plataforma3),
         ("score0",score0),
         ("score100",score100),
         ("score500",score500),
         ("score1000",score1000),
         ("score1500",score1500),
         ("score5000",score5000),
         ("level1",level1),
         ("fogo",fogo),
         ("passaro",passaro),
         ("anao",anao)
         ]}




        




