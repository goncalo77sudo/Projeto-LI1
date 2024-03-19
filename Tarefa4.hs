{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Gonçalo Francisco Loureiro Silva <a106811@alunos.uminho.pt>
              Gonçalo da Costa Sá <a107376@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe

import LI12324
import Tarefa1
import Tarefa2
import Tarefa3

{- | A função 'atualiza' recebe uma lista de acoes para os inimigos e uma acao para o jogador e atualiza o jogo com as respetivas 
condicoes dadas para efetuar o movimento com as funcoes das velocidades com auxilio da funcao 'blocoondeboneco' que dado um mapa 
e a posicao do boneco nos da o bloco onde se encontra o boneco

@
blocoondeboneco :: Mapa -> Posicao -> Maybe Bloco 
blocoondeboneco m@(Mapa _ _ blocos) (x, y) = blocoNaPosicao m (x,y)

@

@
atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza acoes acao2 j
    | acao2 == Nothing = j
    | emEscada (jogador j)  && acao2 == Just Subir = j {jogador = velocidadesubir (jogador j)}
    | acao2 == Just Descer && colisoesEscadaDescer (mapa j) (jogador j) = j {jogador = velocidadedescer (jogador j)}  
    | (blocoondeboneco (mapa j)(posicao(jogador j)) == Just Escada && verificaPlataforma (mapa j) (jogador j) || blocoondeboneco (mapa j)(posicao(jogador j)) /= Just Plataforma ) && acao2 == Just AndarDireita && direcao (jogador j) /= Este = j {jogador = velocidadedireita  (jogador j)}
    | (blocoondeboneco (mapa j)(posicao(jogador j)) == Just Escada && verificaPlataforma (mapa j) (jogador j) || blocoondeboneco (mapa j)(posicao(jogador j)) /= Just Plataforma ) && acao2 == Just AndarDireita && direcao (jogador j) == Este = j {jogador = velocidadedireita2  (jogador j)} 
    | ressalta (jogador j) && (colisoesParede (mapa j)(jogador j) || blocoboneco (mapa j)(posicao(jogador j)) /= Just Plataforma) = j {jogador = invertedirecoes  (jogador j)}
    | (blocoondeboneco (mapa j)(posicao(jogador j)) == Just Escada && verificaPlataforma (mapa j) (jogador j) || blocoondeboneco (mapa j)(posicao(jogador j)) /= Just Plataforma ) && acao2 == Just AndarEsquerda && direcao (jogador j) /= Este = j {jogador = velocidadeesquerda2  (jogador j)}
    | (blocoondeboneco (mapa j)(posicao(jogador j)) == Just Escada && verificaPlataforma (mapa j) (jogador j) || blocoondeboneco (mapa j)(posicao(jogador j)) /= Just Plataforma )&& acao2 == Just AndarEsquerda && direcao (jogador j) == Este = j {jogador = velocidadeesquerda  (jogador j)} 
    | ressalta (jogador j) && (colisoesParede (mapa j)(jogador j) || blocoboneco (mapa j)(posicao(jogador j)) /= Just Plataforma) = j {jogador = invertedirecoes  (jogador j)}
    | blocoboneco (mapa j)(posicao(jogador j)) == Just Plataforma && acao2 == Just Saltar = j {jogador = velocidadeSaltar (jogador j)}
    | blocoondeboneco (mapa j)(posicao(jogador j)) /= Just Plataforma && acao2 == Just Parar = j {jogador = velocidadeParar (mapa j) (jogador j)}
    | head (acoes) == Nothing = j {inimigos = velocidadeinimigos2 (inimigos j)}
    | otherwise = j

@

@
velocidadeinimigos :: Personagem -> Personagem
velocidadeinimigos ini | (tipo ini == Fantasma || tipo ini == Passaro)&& direcao ini == Este = (ini {velocidade = (4,snd(velocidade ini))}) 
                                   | (tipo ini == Fantasma || tipo ini == Passaro )&& direcao ini == Oeste = (ini {velocidade = (-4,snd(velocidade ini))})  
                                   | otherwise = ini

@

@
velocidadeinimigos2 :: [Personagem] -> [Personagem]
velocidadeinimigos2 ini = map velocidadeinimigos ini

@

@
velocidadesubir :: Personagem -> Personagem
velocidadesubir p = p {velocidade = (0,-4)}

@

@
velocidadedescer :: Personagem -> Personagem
velocidadedescer p = p {velocidade = (0,4)}

@

@
velocidadedireita :: Personagem -> Personagem
velocidadedireita p | snd (velocidade p) == 0 = p {velocidade = (4,snd (velocidade p)),
                        direcao = Este}
                        | otherwise = p 

@

@
velocidadedireita2 :: Personagem -> Personagem
velocidadedireita2 p | snd (velocidade p) == 0 = p {velocidade = (4,snd (velocidade p))}
                     | otherwise = p

@
velocidadeesquerda :: Personagem -> Personagem
velocidadeesquerda p | snd (velocidade p) == 0 = p {velocidade = (-4,snd (velocidade p)),
                        direcao = Oeste}
                        | otherwise = p

@

@
velocidadeesquerda2 :: Personagem -> Personagem
velocidadeesquerda2 p | snd (velocidade p) == 0 =  p {velocidade = (-4,snd (velocidade p))}
                      | otherwise = p

@

@
velocidadeSaltar :: Personagem -> Personagem
velocidadeSaltar p = p {velocidade = (fst(velocidade p),-5.5)}

@

@
velocidadeParar :: Mapa -> Personagem -> Personagem
velocidadeParar m p | blocoondeboneco m (posicao p) == Just Escada = p {velocidade = (0,0)}
                  | otherwise = p {velocidade = (0,snd(velocidade p))}

@
-}

blocoondeboneco :: Mapa -> Posicao -> Maybe Bloco 
blocoondeboneco m@(Mapa _ _ blocos) (x, y) = blocoNaPosicao m (x,y)



atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza acoes acao2 j
    | acao2 == Nothing = j
    | emEscada (jogador j)  && acao2 == Just Subir = j {jogador = velocidadesubir (jogador j)}
    | acao2 == Just Descer && colisoesEscadaDescer (mapa j) (jogador j) = j {jogador = velocidadedescer (jogador j)}  
    | (blocoondeboneco (mapa j)(posicao(jogador j)) == Just Escada && verificaPlataforma (mapa j) (jogador j) || blocoondeboneco (mapa j)(posicao(jogador j)) /= Just Plataforma ) && acao2 == Just AndarDireita && direcao (jogador j) /= Este = j {jogador = velocidadedireita  (jogador j)}
    | (blocoondeboneco (mapa j)(posicao(jogador j)) == Just Escada && verificaPlataforma (mapa j) (jogador j) || blocoondeboneco (mapa j)(posicao(jogador j)) /= Just Plataforma ) && acao2 == Just AndarDireita && direcao (jogador j) == Este = j {jogador = velocidadedireita2  (jogador j)} 
    | ressalta (jogador j) && (colisoesParede (mapa j)(jogador j) || blocoboneco (mapa j)(posicao(jogador j)) /= Just Plataforma) = j {jogador = invertedirecoes  (jogador j)}
    | (blocoondeboneco (mapa j)(posicao(jogador j)) == Just Escada && verificaPlataforma (mapa j) (jogador j) || blocoondeboneco (mapa j)(posicao(jogador j)) /= Just Plataforma ) && acao2 == Just AndarEsquerda && direcao (jogador j) /= Este = j {jogador = velocidadeesquerda2  (jogador j)}
    | (blocoondeboneco (mapa j)(posicao(jogador j)) == Just Escada && verificaPlataforma (mapa j) (jogador j) || blocoondeboneco (mapa j)(posicao(jogador j)) /= Just Plataforma )&& acao2 == Just AndarEsquerda && direcao (jogador j) == Este = j {jogador = velocidadeesquerda  (jogador j)} 
    | ressalta (jogador j) && (colisoesParede (mapa j)(jogador j) || blocoboneco (mapa j)(posicao(jogador j)) /= Just Plataforma) = j {jogador = invertedirecoes  (jogador j)}
    | blocoboneco (mapa j)(posicao(jogador j)) == Just Plataforma && acao2 == Just Saltar = j {jogador = velocidadeSaltar (jogador j)}
    | blocoondeboneco (mapa j)(posicao(jogador j)) /= Just Plataforma && acao2 == Just Parar = j {jogador = velocidadeParar (mapa j) (jogador j)}
    | head (acoes) == Nothing = j {inimigos = velocidadeinimigos2 (inimigos j)}
    | otherwise = j

velocidadeinimigos :: Personagem -> Personagem
velocidadeinimigos ini | (tipo ini == Fantasma || tipo ini == Passaro)&& direcao ini == Este = (ini {velocidade = (4,snd(velocidade ini))}) 
                                   | (tipo ini == Fantasma || tipo ini == Passaro )&& direcao ini == Oeste = (ini {velocidade = (-4,snd(velocidade ini))})  
                                   | otherwise = ini

velocidadeinimigos2 :: [Personagem] -> [Personagem]
velocidadeinimigos2 ini = map velocidadeinimigos ini


velocidadesubir :: Personagem -> Personagem
velocidadesubir p = p {velocidade = (0,-4)}

velocidadedescer :: Personagem -> Personagem
velocidadedescer p = p {velocidade = (0,4)}

velocidadedireita :: Personagem -> Personagem
velocidadedireita p | snd (velocidade p) == 0 = p {velocidade = (4,snd (velocidade p)),
                        direcao = Este}
                        | otherwise = p 

velocidadedireita2 :: Personagem -> Personagem
velocidadedireita2 p | snd (velocidade p) == 0 = p {velocidade = (4,snd (velocidade p))}
                     | otherwise = p

velocidadeesquerda :: Personagem -> Personagem
velocidadeesquerda p | snd (velocidade p) == 0 = p {velocidade = (-4,snd (velocidade p)),
                        direcao = Oeste}
                        | otherwise = p

velocidadeesquerda2 :: Personagem -> Personagem
velocidadeesquerda2 p | snd (velocidade p) == 0 =  p {velocidade = (-4,snd (velocidade p))}
                      | otherwise = p

velocidadeSaltar :: Personagem -> Personagem
velocidadeSaltar p = p {velocidade = (fst(velocidade p),-5.5)}


velocidadeParar :: Mapa -> Personagem -> Personagem
velocidadeParar m p | blocoondeboneco m (posicao p) == Just Escada = p {velocidade = (0,0)}
                  | otherwise = p {velocidade = (0,snd(velocidade p))}




