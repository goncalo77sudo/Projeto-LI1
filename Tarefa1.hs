{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Gonçalo Francisco Loureiro Silva <a106811@alunos.uminho.pt>
              Gonçalo da Costa Sá <a107376@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324
import GHC.Float
import Graphics.Gloss.Data.Point (pointInBox)


mapaTeste :: Mapa
mapaTeste = Mapa ((2,19), Oeste) (6.5, 3) blocosTeste  
blocosTeste :: [[Bloco]]
blocosTeste = [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio,Vazio,Vazio, Vazio,Vazio,Vazio, Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio,Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio,Vazio,Vazio, Vazio,Vazio,Vazio, Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio,Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio,Vazio,Vazio, Vazio,Vazio,Vazio, Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio,Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio,Vazio,Vazio, Vazio,Vazio,Vazio, Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Plataforma, Plataforma, Plataforma, Plataforma,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Fogo, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Fogo, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma,Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma,Plataforma,Plataforma]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Escada, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio,  Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Escada, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio]
    ,[Vazio, Fogo, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Fogo, Vazio, Vazio, Fogo , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Fogo, Vazio, Vazio, Vazio, Vazio, Vazio , Escada, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma,Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma,Plataforma,Plataforma]
    ,[Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio]
    ,[Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Fogo, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Fogo, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Alcapao, Alcapao, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma,Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma,Plataforma,Plataforma]
    ,[Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Escada,Vazio,Vazio]
    ,[Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Escada,Vazio,Vazio]
    ,[Vazio, Vazio,Vazio, Vazio, Vazio, Plataforma, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Fogo, Vazio, Vazio, Vazio, Vazio, Vazio , Vazio, Vazio, Vazio, Vazio, Escada,Vazio,Vazio]
    ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma,Plataforma, Plataforma, Plataforma,Plataforma, Plataforma, Plataforma,Plataforma,Plataforma]
   ]



{- | A função 'intersecaoTotal' recebe duas hitboxes e verifica se estas se sobrepõem,
com auxílio da função 'intersecao' .
A funcao 'intersecaoTotalListas' recebe uma hitbox e uma lista de hitboxes e verifica se alguma
dessas hitboxes se sobrepõe com a hitbox recebida.

@
intersecaoTotal :: Hitbox -> Hitbox -> Bool
intersecaoTotal h1 h2 = intersecao h1 h2 || intersecao h2 h1
@

@
intersecao:: Hitbox -> Hitbox -> Bool
intersecao((p1,p2), (p3,p4)) ((p5,p6),(p7,p8)) = pointInBox (double2Float p5,double2Float p6) (double2Float p1,double2Float p2) (double2Float p3,double2Float p4) || pointInBox (double2Float p7,double2Float p8) (double2Float p1,double2Float p2) (double2Float p3,double2Float p4) || pointInBox (double2Float p5,double2Float p8) (double2Float p1,double2Float p2) (double2Float p3,double2Float p4) || pointInBox (double2Float p7,double2Float p6) (double2Float p1,double2Float p2) (double2Float p3,double2Float p4)
@

@
intersecaoTotalListas :: Hitbox -> [Hitbox] -> Bool
intersecaoTotalListas hitbox lista = any (intersecaoTotal hitbox) lista

@

-}

intersecaoTotal :: Hitbox -> Hitbox -> Bool
intersecaoTotal h1 h2 = intersecao h1 h2 || intersecao h2 h1

intersecao:: Hitbox -> Hitbox -> Bool
intersecao((p1,p2), (p3,p4)) ((p5,p6),(p7,p8)) = pointInBox (double2Float p5,double2Float p6) (double2Float p1,double2Float p2) (double2Float p3,double2Float p4) || pointInBox (double2Float p7,double2Float p8) (double2Float p1,double2Float p2) (double2Float p3,double2Float p4) || pointInBox (double2Float p5,double2Float p8) (double2Float p1,double2Float p2) (double2Float p3,double2Float p4) || pointInBox (double2Float p7,double2Float p6) (double2Float p1,double2Float p2) (double2Float p3,double2Float p4)

intersecaoTotalListas :: Hitbox -> [Hitbox] -> Bool
intersecaoTotalListas hitbox lista = any (intersecaoTotal hitbox) lista

{- | A função 'gethitbox' recebe um personagem e devolve a sua hitbox.

@
gethitbox :: Personagem -> Hitbox 
gethitbox l = ((fst (posicao l) - fst (tamanho l)/2, snd (posicao l) - snd (tamanho l)/2 ),(fst (posicao l) + fst(tamanho l)/2, snd (posicao l) + snd (tamanho l)/2)) 
@

-}

gethitbox :: Personagem -> Hitbox 
gethitbox l = ((fst (posicao l) - fst (tamanho l)/2, snd (posicao l) - snd (tamanho l)/2 ),(fst (posicao l) + fst(tamanho l)/2, snd (posicao l) + snd (tamanho l)/2))


{- | A função 'colisoesPersonagens' recebe dois personagens e verifica se estes colidem.

@
colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens l l2 = intersecaoTotal (gethitbox l) (gethitbox l2)
@

-}

colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens l l2 = intersecaoTotal (gethitbox l) (gethitbox l2)




{- | A função 'colisoesParede' recebe um mapa e um personagem e verifica se este colide com uma parede ou uma plataforma,
utiliza a função 'getmaphitbox' recebe um mapa e devolve a sua hitbox.
A função 'colidePlataforma' verifica se o personagem colide com uma plataforma com auxilio
da função 'intersecaoTotalListas' que verifica se duas listas de hitboxes se sobrepõem.
A função 'getHitboxPlataforma' recebe uma matriz de blocos e devolve uma lista de hitboxes dessas plataformas

@
colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede m p = colidePlataforma m p || not (intersecaoTotal (getmaphitbox m) (gethitbox p))
@

@
getmaphitbox :: Mapa -> Hitbox 
getmaphitbox (Mapa _ _ blocos) = ((0.0,0.0),(fromIntegral (length (head blocos)),fromIntegral(length blocos)))
@

@
colidePlataforma :: Mapa -> Personagem -> Bool
colidePlataforma m p = intersecaoTotalListas (gethitbox p) (getHitboxPlataformaMapa m)

@

@
getHitboxPlataforma :: [[Bloco]] -> [Hitbox]
getHitboxPlataforma blocos = map getHitboxBloco (tiraposicaobloco Plataforma blocos)

@

@
getHitboxPlataformaMapa :: Mapa -> [Hitbox]
getHitboxPlataformaMapa m@(Mapa _ _ blocos) = getHitboxPlataforma blocos

@

@
getHitboxBloco :: Posicao -> Hitbox
getHitboxBloco p = let tamanho = (1,1) in ((fst p - fst tamanho/2, snd p - snd tamanho /2 ),(fst p + fst tamanho /2, snd p + snd tamanho /2)) 

@

-}

colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede m p = colidePlataforma m p || not (intersecaoTotal (getmaphitbox m) (gethitbox p))


getmaphitbox :: Mapa -> Hitbox 
getmaphitbox (Mapa _ _ blocos) = ((0.0,0.0),(fromIntegral (length (head blocos)),fromIntegral(length blocos)))

colidePlataforma :: Mapa -> Personagem -> Bool
colidePlataforma m p = intersecaoTotalListas (gethitbox p) (getHitboxPlataformaMapa m)

getHitboxPlataforma :: [[Bloco]] -> [Hitbox]
getHitboxPlataforma blocos = map getHitboxBloco (tiraposicaobloco Plataforma blocos)

getHitboxPlataformaMapa :: Mapa -> [Hitbox]
getHitboxPlataformaMapa m@(Mapa _ _ blocos) = getHitboxPlataforma blocos

getHitboxBloco :: Posicao -> Hitbox
getHitboxBloco p = let tamanho = (1,1) in ((fst p - fst tamanho/2, snd p - snd tamanho /2 ),(fst p + fst tamanho /2, snd p + snd tamanho /2)) 

{- | A função 'tiraposicaobloco' recebe um bloco e uma matriz e devolve uma lista com as posicoes
onde esse bloco se encontra com o auxilio da Indices que devolve uma lista dos índices
onde o elemento é encontrado na lista.

@
tiraposicaobloco :: Bloco -> [[Bloco]] -> [Posicao]
tiraposicaobloco bloco blocos = [(fromIntegral linha, fromIntegral coluna) | coluna <- [0 .. colunas - 1], linha <- indices bloco (blocos !! coluna)]
  where
    colunas = length blocos

@

@
indices :: Eq a => a -> [a] -> [Int]
indices x = aux 0
  where
    aux _ [] = []
    aux i (h:t)
      | x == h = i : aux (i + 1) t
      | otherwise = aux (i + 1) t

@
-}

tiraposicaobloco :: Bloco -> [[Bloco]] -> [Posicao]
tiraposicaobloco bloco blocos = [(fromIntegral linha, fromIntegral coluna) | coluna <- [0 .. colunas - 1], linha <- indices bloco (blocos !! coluna)]
  where
    colunas = length blocos

indices :: Eq a => a -> [a] -> [Int]
indices x = aux 0
  where
    aux _ [] = []
    aux i (h:t)
      | x == h = i : aux (i + 1) t
      | otherwise = aux (i + 1) t

{- | A função 'blocoNaPosicao' recebe um mapa e uma posição e devolve o bloco que se encontra nessa posição.

@
blocoNaPosicao :: Mapa -> Posicao -> Maybe Bloco
blocoNaPosicao (Mapa _ _ blocos) (x, y) 
  | l >= 0 && l < length blocos && c < length (head blocos) && c >= 0 = Just (blocos !! l !! c)
  | otherwise = Nothing
    where l = round y
          c = round x 
@

-}

blocoNaPosicao :: Mapa -> Posicao -> Maybe Bloco
blocoNaPosicao (Mapa _ _ blocos) (x, y) 
  | l >= 0 && l < length blocos && c < length (head blocos) && c >= 0 = Just (blocos !! l !! c)
  | otherwise = Nothing
    where l = round y
          c = round x 

