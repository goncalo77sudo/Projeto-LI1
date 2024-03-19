{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Gonçalo Francisco Loureiro Silva <a106811@alunos.uminho.pt>
              Gonçalo da Costa Sá <a107376@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where
import Data.List

import LI12324

import Tarefa1 

joguinho :: Jogo 
joguinho = let (Mapa (pi,di) pf blocos ) = mapaTeste in
   Jogo mapaTeste enemy colecio player1 {posicao = pi, direcao = di}

colecio :: [(Colecionavel,Posicao)]
colecio = [(Martelo,(7,19)),(Moeda,(18,15))]

player1 :: Personagem
player1 = Personagem {velocidade = (0,0),
                     tipo = Jogador,
                     posicao = (24.5,19),
                     direcao = Este,
                     tamanho = (1,1),
                     emEscada = False,
                     ressalta = False,
                     vida = 3,
                     pontos = 0,
                     aplicaDano = (False,0.0)
                     }
enemy :: [Personagem]
enemy = [Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (16,15), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste},
        Personagem {velocidade = (0,0), 
                    tipo = Fantasma, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (25,19), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste},
        Personagem {velocidade = (0,0), 
                    tipo = MacacoMalvado, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (2,6), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste},
        Personagem {velocidade = (0,0), 
                    tipo = Passaro, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (2,17), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste},
        Personagem {velocidade = (0,0), 
                    tipo = Passaro, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (5,13), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste        },
        Personagem {velocidade = (0,0), 
                    tipo = Passaro, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (18,9), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste      },
        Personagem {velocidade = (0,0), 
                    tipo = Passaro, 
                    emEscada = False, 
                    vida = 1, 
                    pontos = 0, 
                    ressalta = True, 
                    posicao = (10,5), 
                    tamanho = (1,1), 
                    aplicaDano = (False, 0), 
                    direcao = Oeste      } ]


{- | A função 1 'validachao' recebe um mapa e verifica se o chao é composto por plataformas.

@
validachao :: Mapa -> Bool 
validachao (Mapa _ _ blocos) = all (==Plataforma) (last blocos)
@

-}

validachao :: Mapa -> Bool 
validachao (Mapa _ _ blocos) = all (==Plataforma) (last blocos)

{- | A função 2 'validapersonagem' recebe um jogador e uma lista de personagens e verifica se o jogador nao ressalta e se todos os outros ressaltam.

@
validapersonagem :: Personagem -> [Personagem] -> Bool
validapersonagem p inimigos =  all ressalta inimigos && not (ressalta p)

@

-}

validapersonagem :: Personagem -> [Personagem] -> Bool
validapersonagem p inimigos =  all ressalta inimigos && not (ressalta p)

{- | A função 3 'validaposicoes' recebe um mapa, um jogador e uma lista de personagens
e verifica se o jogador nao se sobrepoe a nenhum inimigo.
Já a função 'gethitboxposicaoinicial' recebe um personagem e uma posicao inicial  e devolve a sua hitbox.

@
validaposicoes :: Mapa -> Personagem ->  [Personagem] -> Bool 
validaposicoes _ p [] = False
validaposicoes m@(Mapa (pi,_) _ _) p (h:t)
  |not (intersecaoTotal (gethitboxposicaoinicial p pi)(gethitbox h)) = True
  |otherwise = validaposicoes m p t 

@

@
gethitboxposicaoinicial :: Personagem -> Posicao -> Hitbox
gethitboxposicaoinicial per p =  ((fst p - fst (tamanho per)/2, snd p - snd (tamanho per) /2 ),(fst p + fst (tamanho per) /2, snd p + snd (tamanho per) /2)) 

@

-}

validaposicoes :: Mapa -> Personagem ->  [Personagem] -> Bool 
validaposicoes _ p [] = False
validaposicoes m@(Mapa (pi,_) _ _) p (h:t)
  |not (intersecaoTotal (gethitboxposicaoinicial p pi)(gethitbox h)) = True
  |otherwise = validaposicoes m p t 

gethitboxposicaoinicial :: Personagem -> Posicao -> Hitbox
gethitboxposicaoinicial per p =  ((fst p - fst (tamanho per)/2, snd p - snd (tamanho per) /2 ),(fst p + fst (tamanho per) /2, snd p + snd (tamanho per) /2)) 

{- | A função 4 'enemyminimum' recebe uma lista de personagens e verifica se tem pelo menos 2 inimigos.

@
enemyminimum :: [Personagem] -> Bool 
enemyminimum p 
    |length p >= 2 = True 
    |otherwise = False 

@

-}

enemyminimum :: [Personagem] -> Bool 
enemyminimum p 
    |length p >= 2 = True 
    |otherwise = False 

{- | A função 5 'ghostlife' recebe uma lista de personagens e verifica os fantasmas se tem apenas uma vida.

@
ghostlife :: [Personagem] -> Bool 
ghostlife [] = False
ghostlife (h:t) 
   | tipo h == Fantasma && vida h == 1 = True 
   | otherwise = ghostlife t

@

-}

ghostlife :: [Personagem] -> Bool 
ghostlife [] = False
ghostlife (h:t) 
   | tipo h == Fantasma && vida h == 1 = True 
   | otherwise = ghostlife t

{- | A função 6 'validatestairs' recebe um mapa e verifica se as escadas nao comecam/terminem em alcapoes, e pelo menos uma
das suas extremidades é Plataforma. A função 'checkstairs' agrupa as escadas com auxílio da checkstairsAux.

@
validatestairs :: Mapa -> Bool
validatestairs (Mapa _ _ blocos) = all validatestairsaux (checkstairs (tiraposicaobloco Escada blocos))
    where validatestairsaux :: [Posicao] -> Bool
          validatestairsaux posicoes = ((x1,y1-1) `elem` tiraposicaobloco Plataforma blocos &&
                                (x2,y2-1) `elem` tiraposicaobloco Escada blocos) ||
                                ((x2,y2+1) `elem` tiraposicaobloco Plataforma blocos &&
                                (x1,y1+1) `elem` tiraposicaobloco Escada blocos)
            where (x1,y1) = head posicoes
                  (x2,y2) = last posicoes

@

@
checkstairs :: [Posicao] -> [[Posicao]]
checkstairs p = map (\x-> [head x] ++ [last x]) (checkstairsAux (sortOn fst p))

@

@
checkstairsAux :: [Posicao] -> [[Posicao]]
checkstairsAux [] = []
checkstairsAux [x] = [[x]]
checkstairsAux ((x1,y1):xs)
    | (x1,y1+1) `elem` head g = ((x1,y1) : head g) : tail g
    | otherwise = [(x1,y1)] : g
    where g = checkstairsAux xs
@

-}

validatestairs :: Mapa -> Bool
validatestairs (Mapa _ _ blocos) = all validatestairsaux (checkstairs (tiraposicaobloco Escada blocos))
    where validatestairsaux :: [Posicao] -> Bool
          validatestairsaux posicoes = ((x1,y1-1) `elem` tiraposicaobloco Plataforma blocos &&
                                (x2,y2-1) `elem` tiraposicaobloco Escada blocos) ||
                                ((x2,y2+1) `elem` tiraposicaobloco Plataforma blocos &&
                                (x1,y1+1) `elem` tiraposicaobloco Escada blocos)
            where (x1,y1) = head posicoes
                  (x2,y2) = last posicoes

checkstairs :: [Posicao] -> [[Posicao]]
checkstairs p = map (\x-> [head x] ++ [last x]) (checkstairsAux (sortOn fst p))

checkstairsAux :: [Posicao] -> [[Posicao]]
checkstairsAux [] = []
checkstairsAux [x] = [[x]]
checkstairsAux ((x1,y1):xs)
    | (x1,y1+1) `elem` head g = ((x1,y1) : head g) : tail g
    | otherwise = [(x1,y1)] : g
    where g = checkstairsAux xs


{- | A função  'sacaFantasma' recebe uma lista de personagens e devolve uma lista com as posicoes dos fantasmas
e a função 'sacaKong' recebe uma lista de personagens e devolve uma lista com as posicoes dos macacos malvados.
A função 'sacaMoeda' recebe uma lista de coleccionaveis e devolve uma lista com as posicoes das moedas.
A função 'sacaMartelo' recebe uma lista de coleccionaveis e devolve uma lista com as posicoes dos martelos.
A função 'sacaPassaro' recebe uma lista de personagens e devolve uma lista com as posicoes dos passaros.

@
sacaFantasma :: [Personagem] -> [Posicao]
sacaFantasma [] = []
sacaFantasma (p:t) 
   |tipo p == Fantasma = posicao p : sacaFantasma t
   | otherwise = sacaFantasma t

@

@
sacaKong :: [Personagem] -> [Posicao]
sacaKong [] = []
sacaKong (p:t) 
   |tipo p == MacacoMalvado = posicao p : sacaKong t
   | otherwise = sacaKong t

@

@
sacaMoeda :: [(Colecionavel,Posicao)] -> [Posicao]
sacaMoeda [] = []
sacaMoeda ((col,pos):t) 
   |col == Moeda = pos : sacaMoeda t
   | otherwise = sacaMoeda t

@

@
sacaMartelo :: [(Colecionavel,Posicao)] -> [Posicao]
sacaMartelo [] = []
sacaMartelo ((col,pos):t) 
   |col == Martelo = pos : sacaMartelo t
   | otherwise = sacaMartelo t

@

@
sacaPassaro :: [Personagem] -> [Posicao]
sacaPassaro [] = []
sacaPassaro (p:t) 
   |tipo p == Passaro = posicao p : sacaPassaro t
   | otherwise = sacaPassaro t

@

-}

sacaFantasma :: [Personagem] -> [Posicao]
sacaFantasma [] = []
sacaFantasma (p:t) 
   |tipo p == Fantasma = posicao p : sacaFantasma t
   | otherwise = sacaFantasma t

sacaKong :: [Personagem] -> [Posicao]
sacaKong [] = []
sacaKong (p:t) 
   |tipo p == MacacoMalvado = posicao p : sacaKong t
   | otherwise = sacaKong t

sacaMoeda :: [(Colecionavel,Posicao)] -> [Posicao]
sacaMoeda [] = []
sacaMoeda ((col,pos):t) 
   |col == Moeda = pos : sacaMoeda t
   | otherwise = sacaMoeda t

sacaMartelo :: [(Colecionavel,Posicao)] -> [Posicao]
sacaMartelo [] = []
sacaMartelo ((col,pos):t) 
   |col == Martelo = pos : sacaMartelo t
   | otherwise = sacaMartelo t

sacaPassaro :: [Personagem] -> [Posicao]
sacaPassaro [] = []
sacaPassaro (p:t) 
   |tipo p == Passaro = posicao p : sacaPassaro t
   | otherwise = sacaPassaro t

{- | A função 7 'validatrapdoor' recebe um jogador e um mapa e verifica 
se os alcapoes sao mais largos que o jogador com auxilio da funcao 'validatrapdoorAux' que verifica se os alcapoes
sao mais largos que o jogador e a funcao 'contadorTrapddor' que conta o numero de alcapoes a esquerda e a direita


@
validatrapdoor:: Double -> Mapa-> Bool
validatrapdoor comprimento (Mapa _ _ blocos) =
  validatrapdoorAux blocos (\(x, y) -> 1 + contadorTrapddor (x-1, y) blocos (\a -> a - 1) + contadorTrapddor (x+1, y) blocos (+ 1) > ceiling comprimento)

@

@
validatrapdoorAux :: [[Bloco]] -> ((Int, Int) -> Bool) -> Bool
validatrapdoorAux blocos f = and [f (x, y) | x <- [0..length (head blocos) - 1], y <- [0..length blocos - 1]]

@

@
contadorTrapddor :: (Int, Int) -> [[Bloco]] -> (Int -> Int) -> Int
contadorTrapddor (x, y) blocos f
  | y < 0 || y >= length blocos || x < 0 || x >= length (head blocos) = 0
  | otherwise = case blocos !! y !! x of
      Alcapao -> 1 + contadorTrapddor (f x, y) blocos f
      _ -> 0

@

-}
validatrapdoor:: Double -> Mapa-> Bool
validatrapdoor comprimento (Mapa _ _ blocos) =
  validatrapdoorAux blocos (\(x, y) -> 1 + contadorTrapddor (x-1, y) blocos (\a -> a - 1) + contadorTrapddor (x+1, y) blocos (+ 1) > ceiling comprimento)

validatrapdoorAux :: [[Bloco]] -> ((Int, Int) -> Bool) -> Bool
validatrapdoorAux blocos f = and [f (x, y) | x <- [0..length (head blocos) - 1], y <- [0..length blocos - 1]]

contadorTrapddor :: (Int, Int) -> [[Bloco]] -> (Int -> Int) -> Int
contadorTrapddor (x, y) blocos f
  | y < 0 || y >= length blocos || x < 0 || x >= length (head blocos) = 0
  | otherwise = case blocos !! y !! x of
      Alcapao -> 1 + contadorTrapddor (f x, y) blocos f
      _ -> 0

{- | A função 8 'validacolecionajj' recebe um jogo e verifica se existir personagens ou coleccionaveis
“dentro” de plataformas ou alcapoes

@
validacolecionajj :: Jogo -> Bool
validacolecionajj j = validajogador (mapa j) (jogador j) && validaini (mapa j)(inimigos j)
                      && validacoleciona (mapa j)(colecionaveis j)


@

@
validacoleciona :: Mapa -> [(Colecionavel,Posicao)] -> Bool 
validacoleciona m [] = True
validacoleciona m@(Mapa _ _ blocos) ((col,pos):restantes) 
  |blocoNaPosicao m pos /= Just Vazio = False
  |otherwise = validacoleciona m restantes


@

@
validaini :: Mapa -> [Personagem] -> Bool 
validaini m [] = True
validaini m@(Mapa _ _ blocos) (ini:restantes)
  |blocoNaPosicao m (posicao ini) /= Just Vazio = False
  |otherwise = validaini m restantes


@

@
validajogador :: Mapa -> Personagem -> Bool 
validajogador m@(Mapa _ _ blocos) p
  |blocoNaPosicao m (posicao p) /= Just Vazio = False
  |otherwise = True


@

-}

validacolecionajj :: Jogo -> Bool
validacolecionajj j = validajogador (mapa j) (jogador j) && validaini (mapa j)(inimigos j)
                      && validacoleciona (mapa j)(colecionaveis j)

validacoleciona :: Mapa -> [(Colecionavel,Posicao)] -> Bool 
validacoleciona m [] = True
validacoleciona m@(Mapa _ _ blocos) ((col,pos):restantes) 
  |blocoNaPosicao m pos /= Just Vazio = False
  |otherwise = validacoleciona m restantes

validaini :: Mapa -> [Personagem] -> Bool 
validaini m [] = True
validaini m@(Mapa _ _ blocos) (ini:restantes)
  |blocoNaPosicao m (posicao ini) /= Just Vazio = False
  |otherwise = validaini m restantes

validajogador :: Mapa -> Personagem -> Bool 
validajogador m@(Mapa _ _ blocos) p
  |blocoNaPosicao m (posicao p) /= Just Vazio = False
  |otherwise = True

{- | A função final 'valida' recebe um jogo e verifica se este é válido.

@
valida :: Jogo -> Bool
valida j = validachao (mapa j) && validapersonagem (jogador j)(inimigos j)
 && validaposicoes (mapa j)(jogador j)(inimigos j) && enemyminimum (inimigos j) && ghostlife (inimigos j)
 && validatestairs (mapa j) && validatrapdoor (jogador j)(mapa j) && validacolecionajj j

@

-}

valida :: Jogo -> Bool
valida j = validachao (mapa j) && validapersonagem (jogador j)(inimigos j)
 && validaposicoes (mapa j)(jogador j)(inimigos j) && enemyminimum (inimigos j) && ghostlife (inimigos j)
 && validatestairs (mapa j) && validatrapdoor (fst (tamanho (jogador j))) (mapa j)  && validacolecionajj j

