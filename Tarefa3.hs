{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Gonçalo Francisco Loureiro Silva <a106811@alunos.uminho.pt>
              Gonçalo da Costa Sá <a107376@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
import Tarefa1
import Tarefa2
import Data.Maybe (fromJust)




{- | A função 1 'inimigoperdevidajogo' recebe uma lista de personagens e um personagem
e devolve uma lista de personagens com o inimigo que perdeu vida apos ser atingido
com auxilio da funcao getdamagehitbox que retorna a hitbox de dano do personagem.

@
inimigoperdevidajogo :: Jogo -> Jogo 
inimigoperdevidajogo j = j {inimigos = inimigoperdevida (inimigos j)(jogador j)}

@

@
inimigoperdevida :: [Personagem] -> Personagem -> [Personagem]
inimigoperdevida [] _ = []
inimigoperdevida ini@(i1:restantes) p
  |fst (aplicaDano p) && snd(aplicaDano p) > 0 && intersecaoTotal (getdamagehitbox p) (gethitbox i1) = i1 {vida = 0} : inimigoperdevida restantes p 
  |otherwise = i1 : inimigoperdevida restantes p

@
getdamagehitbox :: Personagem -> Hitbox
getdamagehitbox player 
  | direcao player == Oeste = getdamagehitboxAuxWest (gethitbox player)
  | direcao player == Este = getdamagehitboxAuxEast(gethitbox player)
  | direcao player == Norte = getdamagehitboxAuxNorth (gethitbox player)
  | direcao player == Sul =  getdamagehitboxAuxSouth (gethitbox player)
  where getdamagehitboxAuxWest :: Hitbox -> Hitbox 
        getdamagehitboxAuxWest ((x,y),(x2,y2)) = ((x-(x2-x),y),(x2-(x2-x),y2))
        getdamagehitboxAuxEast :: Hitbox -> Hitbox 
        getdamagehitboxAuxEast ((x,y),(x2,y2)) = ((x+(x2-x),y),(x2+(x2-x),y2))
        getdamagehitboxAuxNorth :: Hitbox -> Hitbox 
        getdamagehitboxAuxNorth ((x,y),(x2,y2)) = ((x,y+(y2-y)),(x2,y2+(y2-y)))
        getdamagehitboxAuxSouth :: Hitbox -> Hitbox 
        getdamagehitboxAuxSouth ((x,y),(x2,y2)) = ((x,y-(y2-y)),(x2,y2-(y2-y)))
@

-}

inimigoperdevidajogo :: Jogo -> Jogo 
inimigoperdevidajogo j = j {inimigos = inimigoperdevida (inimigos j)(jogador j)}

inimigoperdevida :: [Personagem] -> Personagem -> [Personagem]
inimigoperdevida [] _ = []
inimigoperdevida ini@(i1:restantes) p
  |fst (aplicaDano p) && snd(aplicaDano p) > 0 && intersecaoTotal (getdamagehitbox p) (gethitbox i1) = i1 {vida = 0} : inimigoperdevida restantes p 
  |otherwise = i1 : inimigoperdevida restantes p

getdamagehitbox :: Personagem -> Hitbox
getdamagehitbox player 
  | direcao player == Oeste = getdamagehitboxAuxWest (gethitbox player)
  | direcao player == Este = getdamagehitboxAuxEast(gethitbox player)
  | direcao player == Norte = getdamagehitboxAuxNorth (gethitbox player)
  | direcao player == Sul =  getdamagehitboxAuxSouth (gethitbox player)
  where getdamagehitboxAuxWest :: Hitbox -> Hitbox 
        getdamagehitboxAuxWest ((x,y),(x2,y2)) = ((x-(x2-x),y),(x2-(x2-x),y2))
        getdamagehitboxAuxEast :: Hitbox -> Hitbox 
        getdamagehitboxAuxEast ((x,y),(x2,y2)) = ((x+(x2-x),y),(x2+(x2-x),y2))
        getdamagehitboxAuxNorth :: Hitbox -> Hitbox 
        getdamagehitboxAuxNorth ((x,y),(x2,y2)) = ((x,y+(y2-y)),(x2,y2+(y2-y)))
        getdamagehitboxAuxSouth :: Hitbox -> Hitbox 
        getdamagehitboxAuxSouth ((x,y),(x2,y2)) = ((x,y-(y2-y)),(x2,y2-(y2-y)))

{- | A função 2 'inimigomorrejogo' recebe um jogo e devolve um jogo com o inimigo
que morreu apos ser atingido com auxilio da funcao inimigomorre que retorna a posicao
(1000,1000) do inimigo que morreu.

@
inimigomorrejogo :: Jogo -> Jogo
inimigomorrejogo j = j {inimigos = inimigomorre (inimigos j)}

@

@
inimigomorre:: [Personagem] -> [Personagem]
inimigomorre p = map inimigoNewPosition p

@

@
inimigoNewPosition :: Personagem -> Personagem 
inimigoNewPosition p | tipo p == Fantasma && vida p == 0 = p { posicao = (1000,1000)}
                     | otherwise = p 

@

-}

inimigomorrejogo :: Jogo -> Jogo
inimigomorrejogo j = j {inimigos = inimigomorre (inimigos j)}

inimigomorre:: [Personagem] -> [Personagem]
inimigomorre p = map inimigoNewPosition p

inimigoNewPosition :: Personagem -> Personagem 
inimigoNewPosition p | (tipo p == Fantasma || tipo p == Passaro) && vida p == 0 = p { posicao = (1000,1000)}
                     | otherwise = p 

{- | A função 3 'aplicaGravidadej' recebe um tempo e um jogo e devolve um jogo apos aplicacao da gra
vidade ao jogador com auxilio da funcao aplicaGravidade que se o jogador nao estiver numa plataforma
irá "cair" até uma nova plataforma.

@
aplicaGravidadej :: Tempo -> Jogo -> Jogo 
aplicaGravidadej t j = j {jogador = aplicaGravidade t (jogador j) (mapa j)}

@

@
aplicaGravidade :: Tempo -> Personagem -> Mapa -> Personagem
aplicaGravidade t p (Mapa _ _ []) = p
aplicaGravidade t p (Mapa _ _ [[]]) = p
aplicaGravidade t p m@(Mapa _ _ blocos)
   | not (estaemPlataforma m p) && not (emEscada p) = (p { velocidade = novaPosicaoaux2 t (velocidade p) })
   | otherwise = p {velocidade = (fst(velocidade p),if (snd(velocidade p) < 0) || ((emEscada p  &&  verificaPlataforma m p && blocobonecoBaixo m (posicao p) /= Just Vazio && snd(velocidade p) >= 0 ) || ((emEscada p && not (verificaPlataforma m p) && snd(velocidade p) >= 0 ))) then snd(velocidade p) else 0)}

@

@
novaPosicaoaux2 :: Tempo -> Posicao -> Posicao
novaPosicaoaux2 t (x,y) = (x,y + (t * snd gravidade))

@

-}

aplicaGravidadej :: Tempo -> Jogo -> Jogo 
aplicaGravidadej t j = j {jogador = aplicaGravidade t (jogador j) (mapa j)}

aplicaGravidade :: Tempo -> Personagem -> Mapa -> Personagem
aplicaGravidade t p (Mapa _ _ []) = p
aplicaGravidade t p (Mapa _ _ [[]]) = p
aplicaGravidade t p m@(Mapa _ _ blocos)
   | not (estaemPlataforma m p) && not (emEscada p) = (p { velocidade = novaPosicaoaux2 t (velocidade p) })
   | otherwise = p {velocidade = (fst(velocidade p),if (snd(velocidade p) < 0) || ((emEscada p  &&  verificaPlataforma m p && blocobonecoBaixo m (posicao p) /= Just Vazio && snd(velocidade p) >= 0 ) || ((emEscada p && not (verificaPlataforma m p) && snd(velocidade p) >= 0 ))) then snd(velocidade p) else 0)}
-- saltar,escada e plataforma subir,esta em escada e nao esta plataforma em baixo e descer,esta em escada e esta plataforma em baixo e descer

novaPosicaoaux2 :: Tempo -> Velocidade -> Velocidade
novaPosicaoaux2 t (x,y) = (x,y + (t * snd gravidade))

estaEmEscada :: Mapa -> Personagem -> Bool 
estaEmEscada a@(Mapa _ _ blocos) p | blocoNaPosicao a (posicao p) == Just Escada = True
                                   | otherwise = False

verificaPlataforma :: Mapa -> Personagem -> Bool 
verificaPlataforma a@(Mapa _ _ blocos) p | blocoNaPosicao a (fst(posicao p), (snd (posicao p))+0.5) == Just Plataforma = True 
                                         | otherwise = False

{- | As funcoes 'estaemPlataforma' e 'estaemAlcapao' recebem um mapa e um personagem e verificam 
se o personagem esta sobre uma plataforma  com auxilio da funcao 'blocoboneco' que devolve
o bloco que se encontra na posicao abaixo do personagem.

@
estaemPlataforma :: Mapa -> Personagem -> Bool
estaemPlataforma m@(Mapa _ _ blocos) p = blocoatual == Just Plataforma 
          where (a,b) = posicao p 
                blocoatual = blocoboneco m (a,b)

@

@
blocobonecoBaixo :: Mapa -> Posicao -> Maybe Bloco
blocobonecoBaixo m@(Mapa _ _ blocos) (x, y) = blocoNaPosicao m (x,y+2)

@

@
blocoboneco :: Mapa -> Posicao -> Maybe Bloco 
blocoboneco m@(Mapa _ _ blocos) (x, y) = blocoNaPosicao m (x,y+1)

@

-}

estaemPlataforma :: Mapa -> Personagem -> Bool
estaemPlataforma m@(Mapa _ _ blocos) p = blocoatual == Just Plataforma 
          where (a,b) = posicao p 
                blocoatual = blocoboneco m (a,b)

blocobonecoBaixo :: Mapa -> Posicao -> Maybe Bloco
blocobonecoBaixo m@(Mapa _ _ blocos) (x, y) = blocoNaPosicao m (x,y+2)

blocoboneco :: Mapa -> Posicao -> Maybe Bloco 
blocoboneco m@(Mapa _ _ blocos) (x, y) = blocoNaPosicao m (x,y+0.5)

{- | A funcao playerhitjj recebe um jogo e devolve um jogo com mudancas feitas no jogador,
sendo essas mudancas a perda de vida do jogador se este colidir com um inimigo 
com auxilio da funcao 'playerhit' que devolve o jogador com menos vida se este , retornando
a posicao inicial

@
playerhitjj :: Jogo -> Jogo 
playerhitjj j = j {jogador = playerhit (jogador j) (mapa j)  (inimigos j)}

@

@
playerhit :: Personagem -> Mapa  -> [Personagem] -> Personagem
playerhit p m [] = p
playerhit p m@(Mapa (pi,_) _ _ )  (ini:restantes)
  | colisoesPersonagens p ini && not (fst (aplicaDano p)) = p {vida = (vida p - 1), posicao = pi}
  | otherwise = playerhit p m restantes

@

-}

playerhit :: Personagem -> Mapa  -> [Personagem] -> Personagem
playerhit p m [] = p
playerhit p m@(Mapa (pi,_) _ _ )  (ini:restantes)
  | colisoesPersonagens p ini && not (fst (aplicaDano p)) = p {vida = (vida p - 1), posicao = pi}
  | blocoNaPosicao m (posicao p) == Just Fogo = p {vida = (vida p - 1), posicao = pi}
  | otherwise = playerhit p m restantes

playerhitjj :: Jogo -> Jogo 
playerhitjj j = j {jogador = playerhit (jogador j) (mapa j)  (inimigos j)}

{- | A funcao 5 'apllicacolecio' recebe um jogo e devolve um jogo com mudancas feitas no jogador
e nos colecionaveis com auxilio das funcoes 'ak47xp' que devolve o jogador com o martelo armado
e com mais pontuacao se apanhar a moeda e 'removecolecio' que devolve a lista de colecionaveis
com o colecionavel que foi apanhado removido

@
aplicacolecio :: Jogo -> Jogo 
aplicacolecio j = j { 
  jogador = ak47xp (jogador j)(colecionaveis j),
  colecionaveis = removecolecio (jogador j) (colecionaveis j)
}

@

@
removecolecio :: Personagem -> [(Colecionavel,Posicao)] -> [(Colecionavel,Posicao)]
removecolecio p [] = []
removecolecio p (((col,pos):t)) 
  |intersecaoTotal (gethitbox p) (getHitboxBloco pos) = (col,desaparececolecioaux pos) : removecolecio p t
  |otherwise = (col,pos) : removecolecio p t

@

@
ak47xp :: Personagem -> [(Colecionavel,Posicao)] -> Personagem
ak47xp p [] = p
ak47xp p (((col,pos):t)) 
  |intersecaoTotal (gethitbox p) (getHitboxBloco pos) && col == Martelo = p {aplicaDano = (True,10)} 
  |intersecaoTotal (gethitbox p) (getHitboxBloco pos) && col == Moeda = p {pontos = pontos p + 10} 
  |otherwise = ak47xp p t 

@

@
desaparececolecio :: Personagem -> Posicao -> Posicao
desaparececolecio p c
 |intersecaoTotal (gethitbox p) (getHitboxBloco c) = desaparececolecioaux c
 |otherwise = c

@

@
desaparececolecioaux:: Posicao -> Posicao
desaparececolecioaux (x,y) = (x+1000,y+1000)

@

-}

aplicacolecio :: Tempo -> Jogo -> Jogo 
aplicacolecio t j = j { 
  jogador = ak47xp t (jogador j)(colecionaveis j),
  colecionaveis = removecolecio (jogador j) (colecionaveis j)
}

removecolecio :: Personagem -> [(Colecionavel,Posicao)] -> [(Colecionavel,Posicao)]   
removecolecio p [] = []
removecolecio p (((col,pos):t)) 
  |intersecaoTotal (gethitbox p) (getHitboxBloco pos) = (col,desaparececolecioaux pos) : removecolecio p t
  |otherwise = (col,pos) : removecolecio p t 

ak47xp :: Tempo -> Personagem -> [(Colecionavel,Posicao)] -> Personagem
ak47xp te p [] = p {aplicaDano = (snd (aplicaDano p)>0 , if snd (aplicaDano p)<=0 then 0 else snd (aplicaDano p)-te)}
ak47xp te p (((col,pos):t)) 
  |intersecaoTotal (gethitbox p) (getHitboxBloco pos) && col == Martelo = p {aplicaDano = (True,10)} 
  |intersecaoTotal (gethitbox p) (getHitboxBloco pos) && col == Moeda = p {pontos = pontos p + 10} 
  |otherwise = ak47xp te p t 


desaparececolecioaux:: Posicao -> Posicao
desaparececolecioaux (x,y) = (x+1000,y+1000)


{- | A funcao 6 'alcapaoJogo' recebe um jogo e devolve um jogo com mudancas feitas no mapa,
com auxilio da funcao 'trocablocoPorLinha' que devolve o mapa com a matriz alterada na primeira linha e da funcao
'trocaBloco' que transforma essa mudanca e aplica para as colunas restantes.

@
alcapaoJogo :: Jogo -> Jogo
alcapaoJogo j = let (Mapa (pi,di) pf blocos) = mapa j
  in j {mapa = Mapa (pi,di) pf (trocaBloco (0,0) (jogador j) blocos)}

@

@
trocaBloco :: Posicao -> Personagem -> [[Bloco]] -> [[Bloco]]
trocaBloco _ _ [] = []
trocaBloco (p1, p2) p (linha:restantes) =
  trocaBlocoPorLinha (p1, p2) p linha : trocaBloco (p1, p2 + 1) p restantes

@

@
trocaBlocoPorLinha :: Posicao -> Personagem -> [Bloco] -> [Bloco]
trocaBlocoPorLinha _ _ [] = []
trocaBlocoPorLinha (p1, p2) p (bloco:restantes)
  | isJogador && bloco == Alcapao && intersecaoTotal hitboxBloco hitboxJogador =
    Vazio : trocaBlocoPorLinha (p1 + 1, p2) p restantes
  | otherwise = bloco : trocaBlocoPorLinha (p1 + 1, p2) p restantes
  where
    hitboxBloco = getHitboxBloco (p1, p2)
    hitboxJogador = gethitbox p
    isJogador = tipo p == Jogador
@

-}

alcapaoJogo :: Jogo -> Jogo
alcapaoJogo j = let (Mapa (pi,di) pf blocos) = mapa j
  in j {mapa = Mapa (pi,di) pf (trocaBloco (0,0) (jogador j) blocos)}

trocaBloco :: Posicao -> Personagem -> [[Bloco]] -> [[Bloco]]
trocaBloco _ _ [] = []
trocaBloco (p1, p2) p (linha:restantes) =
  trocaBlocoPorLinha (p1, p2) p linha : trocaBloco (p1, p2 + 1) p restantes

trocaBlocoPorLinha :: Posicao -> Personagem -> [Bloco] -> [Bloco]
trocaBlocoPorLinha _ _ [] = []
trocaBlocoPorLinha (p1, p2) p (bloco:restantes)
  | isJogador && bloco == Alcapao && intersecaoTotal hitboxBloco hitboxJogador =
    Vazio : trocaBlocoPorLinha (p1 + 1, p2) p restantes
  | otherwise = bloco : trocaBlocoPorLinha (p1 + 1, p2) p restantes
  where
    hitboxBloco = getHitboxBloco (p1, p2)
    hitboxJogador = gethitbox p
    isJogador = tipo p == Jogador

{- | A funcao 7 'nocrossing' recebe um jogo e devolve um jogo com mudancas feitas nos personagens,
com auxilio das funcoes 'maplimit' e 'maplimitghost' que devolvem os personagens com a posicao
alterada caso estes ultrapassem os limites do mapa.

@
nocrossing :: Jogo -> Jogo 
nocrossing j = j {
  jogador = maplimit (jogador j)(mapa j),
  inimigos = maplimitghost (inimigos j)(mapa j)
  }

@

@
maplimit :: Personagem -> Mapa -> Personagem
maplimit p m@(Mapa _ _ blocos) 
  | fst (posicao p)  < 0 &&  direcao p /= Este = p {posicao = (0,snd (posicao p))}
  | fst (posicao p)  > fromIntegral (length (head blocos)) - 1.5 = p {posicao = (fromIntegral (length (head blocos))-1.5,snd (posicao p))}
  | snd (posicao p)  < 0 = p {posicao = (fst (posicao p),0)}
  | snd (posicao p)  > fromIntegral (length blocos) + 1.5 = p {posicao = (fst (posicao p),fromIntegral (length blocos) + 1.5)}
  | blocoNaPosicao m (fst (posicao p)-0.5,snd (posicao p)) == Just Plataforma && direcao p == Oeste && not (emEscada p) && not (blocoNaPosicao m (fst (posicao p),snd (posicao p)+1) == Just Escada) = p {posicao = (fst (posicao p)+0.5,snd (posicao p)),direcao = Este}
  | blocoNaPosicao m (fst (posicao p)-0.5,snd (posicao p)) == Just Plataforma && direcao p == Este && not (emEscada p) && not (blocoNaPosicao m (fst (posicao p),snd (posicao p)+1) == Just Escada)= p {posicao = (fst (posicao p)+0.5,snd (posicao p)),direcao = Oeste}
  | blocoNaPosicao m (fst (posicao p)+0.5,snd (posicao p)) == Just Plataforma && direcao p == Este && not (emEscada p) && not (blocoNaPosicao m (fst (posicao p),snd (posicao p)+1) == Just Escada) = p {posicao = (fst (posicao p)-0.5,snd (posicao p)),direcao = Oeste}
  | blocoNaPosicao m (fst (posicao p)+0.5,snd (posicao p)) == Just Plataforma && direcao p == Oeste && not (emEscada p) && not (blocoNaPosicao m (fst (posicao p),snd (posicao p)+1) == Just Escada) = p {posicao = (fst (posicao p)-0.5,snd (posicao p)),direcao = Este}
  | otherwise = p 


@

@
maplimitghost :: [Personagem] -> Mapa -> [Personagem] 
maplimitghost inimigo m@(Mapa _ _ blocos) = map (\x -> maplimit x m) inimigo

@

-}

nocrossing :: Jogo -> Jogo 
nocrossing j = j {
  jogador = maplimit (jogador j)(mapa j),
  inimigos = maplimitghost3 (inimigos j)(mapa j)
  }

maplimit :: Personagem -> Mapa -> Personagem
maplimit p m@(Mapa _ _ blocos) 
  | fst (posicao p)  < 0 &&  direcao p /= Este = p {posicao = (0,snd (posicao p))}
  | fst (posicao p)  > fromIntegral (length (head blocos)) - 1.5 = p {posicao = (fromIntegral (length (head blocos))-1.5,snd (posicao p))}
  | snd (posicao p)  < 0 = p {posicao = (fst (posicao p),0)}
  | snd (posicao p)  >= fromIntegral (length blocos) -1.1 = p {posicao = (fst (posicao p),fromIntegral (length blocos)+1.1)}
  | blocoNaPosicao m (fst (posicao p)-0.5,snd (posicao p)) == Just Plataforma && direcao p == Oeste && not (emEscada p) && not (blocoNaPosicao m (fst (posicao p),snd (posicao p)+1) == Just Escada) = p {posicao = (fst (posicao p)+0.5,snd (posicao p)),direcao = Este}
  | blocoNaPosicao m (fst (posicao p)-0.5,snd (posicao p)) == Just Plataforma && direcao p == Este && not (emEscada p) && not (blocoNaPosicao m (fst (posicao p),snd (posicao p)+1) == Just Escada)= p {posicao = (fst (posicao p)+0.5,snd (posicao p)),direcao = Oeste}
  | blocoNaPosicao m (fst (posicao p)+0.5,snd (posicao p)) == Just Plataforma && direcao p == Este && not (emEscada p) && not (blocoNaPosicao m (fst (posicao p),snd (posicao p)+1) == Just Escada) = p {posicao = (fst (posicao p)-0.5,snd (posicao p)),direcao = Oeste}
  | blocoNaPosicao m (fst (posicao p)+0.5,snd (posicao p)) == Just Plataforma && direcao p == Oeste && not (emEscada p) && not (blocoNaPosicao m (fst (posicao p),snd (posicao p)+1) == Just Escada) = p {posicao = (fst (posicao p)-0.5,snd (posicao p)),direcao = Este}
  | otherwise = p 

maplimitghost :: Personagem -> Mapa -> Personagem 
maplimitghost p m@(Mapa _ _ blocos) 
  | (tipo p == Fantasma || tipo p == Passaro ) && fst (posicao p)  <= 0 &&  direcao p /= Este = p {velocidade = (negate (fst(velocidade p)),snd(velocidade p))} 
  | (tipo p == Fantasma || tipo p == Passaro ) && fst (posicao p)  >= fromIntegral (length (head blocos)) - 1.5 = p {posicao = (fromIntegral (length (head blocos))-1.5,snd (posicao p)),velocidade = (negate (fst(velocidade p)),snd(velocidade p))} 
  | (tipo p == Fantasma || tipo p == Passaro ) &&blocoNaPosicao m (fst (posicao p)-0.5,snd (posicao p)) == Just Plataforma && direcao p == Oeste && not (emEscada p) && not (blocoNaPosicao m (fst (posicao p),snd (posicao p)+1) == Just Escada) = p {posicao = (fst (posicao p)+0.5,snd (posicao p)),direcao = Este,velocidade = (negate (fst(velocidade p)),snd(velocidade p))} 
  | (tipo p == Fantasma || tipo p == Passaro ) && blocoNaPosicao m (fst (posicao p)-0.5,snd (posicao p)) == Just Plataforma && direcao p == Este && not (emEscada p) && not (blocoNaPosicao m (fst (posicao p),snd (posicao p)+1) == Just Escada)= p {posicao = (fst (posicao p)+0.5,snd (posicao p)),direcao = Oeste,velocidade = (negate (fst(velocidade p)),snd(velocidade p))} 
  | (tipo p == Fantasma || tipo p == Passaro ) && blocoNaPosicao m (fst (posicao p)+0.5,snd (posicao p)) == Just Plataforma && direcao p == Este && not (emEscada p) && not (blocoNaPosicao m (fst (posicao p),snd (posicao p)+1) == Just Escada) = p {posicao = (fst (posicao p)-0.5,snd (posicao p)),direcao = Oeste,velocidade = (negate (fst(velocidade p)),snd(velocidade p))} 
  | (tipo p == Fantasma || tipo p == Passaro ) && blocoNaPosicao m (fst (posicao p)+0.5,snd (posicao p)) == Just Plataforma && direcao p == Oeste && not (emEscada p) && not (blocoNaPosicao m (fst (posicao p),snd (posicao p)+1) == Just Escada) = p {posicao = (fst (posicao p)-0.5,snd (posicao p)),direcao = Este, velocidade = (negate (fst(velocidade p)),snd(velocidade p))} 
  | otherwise = p 

maplimitghost3 :: [Personagem] -> Mapa -> [Personagem]
maplimitghost3 ini m = map (\x -> maplimitghost x m) ini


{- | A função final 'movimenta' recebe uma semente, um tempo e um jogo e devolve um jogo aplicando
as funcoes anteriores em cadeia.

@
movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta s t j = movimentacaoPersonagensFinal (mapa j) (1/60) $ escadasJogo $ inimigoperdevidajogo $ inimigomorrejogo $ aplicaGravidadej t $ playerhitjj $ aplicacolecio $ alcapaoJogo $ nocrossing  j

@

-}

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta s t j = movimentacaoPersonagensFinal (mapa j) (1/60) $ escadasJogo $ inimigoperdevidajogo $ inimigomorrejogo $ aplicaGravidadej t $ playerhitjj $ aplicacolecio t $ alcapaoJogo $ nocrossing  j

movimentacaoPersonagensFinal:: Mapa-> Tempo -> Jogo -> Jogo
movimentacaoPersonagensFinal mapa t j = j{ jogador = movimentacaoPersonagens t mapa (jogador j), inimigos = map (movimentacaoPersonagens t mapa) (inimigos j)}

movimentacaoPersonagens :: Tempo -> Mapa -> Personagem -> Personagem
movimentacaoPersonagens t mapa p =
    let (x, y) = posicao p
        (vx, vy) = velocidade p
        (gx, gy) = gravidade
    in
    if colisoesEscadaSubir mapa p && direcao p == Norte
        then p { posicao = (x, y + vy * t), velocidade = (0, vy) }
    else if colisoesEscadaDescer mapa p && direcao p == Sul
        then p { posicao = (x, y + vy * t), velocidade = (0, vy) }
    else
        p { posicao = (x + vx * t, y + vy * t) }


colisoesEscada :: Mapa -> Personagem -> Bool 
colisoesEscada (Mapa _ _ blocos) p
    | null $ foldl (\x y -> if intersecaoTotal (gethitbox p) y then y : x else x) [] (hitboxEscadasTotal blocos (0, 0)) = False
    | otherwise = True

colisoesEscada2 :: Mapa -> Personagem -> Bool 
colisoesEscada2 (Mapa _ _ blocos) p
    | null $ foldl (\x y -> if intersecaoTotal (gethitbox p) y then y : x else x) [] (hitboxEscadasTotal blocos (0, 0)) = False
    | otherwise = True

colisoesEscadaSubir :: Mapa -> Personagem -> Bool
colisoesEscadaSubir (Mapa _ _ blocos) p
    | null $ foldl (\x y -> if intersecaoTotal (gethitbox p) y then y : x else x) [] (hitboxEscadas2 blocos (0, 0)) = False
    | otherwise = True

colisoesEscadaDescer :: Mapa -> Personagem -> Bool
colisoesEscadaDescer (Mapa _ _ blocos) p
    | null $ foldl (\x y -> if intersecaoTotal (gethitbox p) y then y : x else x) [] (hitboxEscadasNovas blocos (0, 0)) = False
    | otherwise = True


hitboxEscadasTotal :: [[Bloco]] -> Posicao-> [Hitbox]
hitboxEscadasTotal blocos pos = hitboxEscadas2 blocos pos ++ hitboxEscadasNovas blocos pos

hitboxEscadaSubir :: Posicao -> Hitbox
hitboxEscadaSubir (x,y) = ((x - t/2,y-1-t/2),(x+t/2,y+t/2))
    where t = 1

hitboxEscadasLInha2 :: Posicao -> [Bloco] -> [Hitbox] 
hitboxEscadasLInha2 (x,y)  [] = []
hitboxEscadasLInha2 (x,y)  (h:t) = if h == Escada then hitboxEscadaSubir (x,y) : hitboxEscadasLInha2 (x+1,y) t else hitboxEscadasLInha2 (x+1,y) t

hitboxEscadas2 :: [[Bloco]] -> Posicao -> [Hitbox]
hitboxEscadas2 [] _ = []
hitboxEscadas2 (h:t) (x,y) = hitboxEscadasLInha2 (x,y) h ++ hitboxEscadas2 t (x,y+1) 

hitboxEscadaDescer :: Posicao -> Hitbox
hitboxEscadaDescer (x,y) = let t = 1 in ((x - t/2,y-1-t/2),(x+t/2,y-0.3))


hitboxEscadasNovasLInha :: Posicao -> [Bloco]  -> [Hitbox]
hitboxEscadasNovasLInha (x,y)  [] = []
hitboxEscadasNovasLInha (x,y)  (h:t) = if h == Escada then hitboxEscadaDescer (x,y) : hitboxEscadasNovasLInha (x+1,y) t else hitboxEscadasNovasLInha (x+1,y) t

hitboxEscadasNovas :: [[Bloco]] ->  Posicao ->  [Hitbox]
hitboxEscadasNovas [] _ = []
hitboxEscadasNovas (h:t) (x,y) = hitboxEscadasNovasLInha (x,y) h ++ hitboxEscadasNovas t (x,y+1)

{- | As seguintes funcoes não estão no enunciado do projeto, mas foram criadas para auxiliar
o movimento dos personagens no jogo.
A funcao 'novaPosicao11' recebe uma velocidade, um tempo e uma posicao e devolve uma nova posicao
com a velocidade aplicada.
A funcao validaEmEscada recebe um mapa e um personagem e devolve o personagem com o atributo 
emEscada alterado caso este esteja numa escada.
A funcao gethitboxEscadaMapa recebe um mapa e devolve uma lista de hitboxes de escadas.
A funcao invertedirecoes recebe um personagem e devolve o personagem com a direcao alterada.

@
movimentacaoPersonagensFinal:: Mapa-> Tempo -> Jogo -> Jogo
movimentacaoPersonagensFinal mapa t j = j{ jogador = movimentacaoPersonagens t mapa (jogador j), inimigos = map (movimentacaoPersonagens t mapa) (inimigos j)}

@

@
movimentacaoPersonagens :: Tempo -> Mapa -> Personagem -> Personagem
movimentacaoPersonagens t mapa p =
    let (x, y) = posicao p
        (vx, vy) = velocidade p
        (gx, gy) = gravidade
    in
    if colisoesEscadaSubir mapa p && direcao p == Norte
        then p { posicao = (x, y + vy * t), velocidade = (0, vy) }
    else if colisoesEscadaDescer mapa p && direcao p == Sul
        then p { posicao = (x, y + vy * t), velocidade = (0, vy) }
    else
        p { posicao = (x + vx * t, y + vy * t) }

@

@
colisoesEscada :: Mapa -> Personagem -> Bool 
colisoesEscada (Mapa _ _ blocos) p
    | null $ foldl (\x y -> if intersecaoTotal (gethitbox p) y then y : x else x) [] (hitboxEscadasTotal blocos (0, 0)) = False
    | otherwise = True

@
colisoesEscada2 :: Mapa -> Personagem -> Bool 
colisoesEscada2 (Mapa _ _ blocos) p
    | null $ foldl (\x y -> if intersecaoTotal (gethitbox p) y then y : x else x) [] (hitboxEscadasTotal blocos (0, 0)) = False
    | otherwise = True

@

@
colisoesEscadaSubir :: Mapa -> Personagem -> Bool
colisoesEscadaSubir (Mapa _ _ blocos) p
    | null $ foldl (\x y -> if intersecaoTotal (gethitbox p) y then y : x else x) [] (hitboxEscadas2 blocos (0, 0)) = False
    | otherwise = True

@

@
colisoesEscadaDescer :: Mapa -> Personagem -> Bool
colisoesEscadaDescer (Mapa _ _ blocos) p
    | null $ foldl (\x y -> if intersecaoTotal (gethitbox p) y then y : x else x) [] (hitboxEscadasNovas blocos (0, 0)) = False
    | otherwise = True

@

@
hitboxEscadasTotal :: [[Bloco]] -> Posicao-> [Hitbox]
hitboxEscadasTotal blocos pos = hitboxEscadas2 blocos pos ++ hitboxEscadasNovas blocos pos

@

@
hitboxEscadaSubir :: Posicao -> Hitbox
hitboxEscadaSubir (x,y) = ((x - t/2,y-1-t/2),(x+t/2,y+t/2))
    where t = 1

@

@
hitboxEscadasLInha2 :: Posicao -> [Bloco] -> [Hitbox] 
hitboxEscadasLInha2 (x,y)  [] = []
hitboxEscadasLInha2 (x,y)  (h:t) = if h == Escada then hitboxEscadaSubir (x,y) : hitboxEscadasLInha2 (x+1,y) t else hitboxEscadasLInha2 (x+1,y) t

@

@
hitboxEscadas2 :: [[Bloco]] -> Posicao -> [Hitbox]
hitboxEscadas2 [] _ = []
hitboxEscadas2 (h:t) (x,y) = hitboxEscadasLInha2 (x,y) h ++ hitboxEscadas2 t (x,y+1) 

@

@
hitboxEscadaDescer :: Posicao -> Hitbox
hitboxEscadaDescer (x,y) = let t = 1 in ((x - t/2,y-1-t/2),(x+t/2,y-0.3))

@

@
hitboxEscadasNovasLInha :: Posicao -> [Bloco]  -> [Hitbox]
hitboxEscadasNovasLInha (x,y)  [] = []
hitboxEscadasNovasLInha (x,y)  (h:t) = if h == Escada then hitboxEscadaDescer (x,y) : hitboxEscadasNovasLInha (x+1,y) t else hitboxEscadasNovasLInha (x+1,y) t

@

@
hitboxEscadasNovas :: [[Bloco]] ->  Posicao ->  [Hitbox]
hitboxEscadasNovas [] _ = []
hitboxEscadasNovas (h:t) (x,y) = hitboxEscadasNovasLInha (x,y) h ++ hitboxEscadasNovas t (x,y+1)

@

@
validaEmEscada:: Mapa -> Personagem -> Personagem
validaEmEscada mapa jogador | colideEscada mapa jogador = jogador{emEscada=True} 
                            | otherwise = jogador{emEscada=False}

@

@
escadasJogo :: Jogo -> Jogo
escadasJogo jogo = jogo {jogador = validaEmEscada (mapa jogo)(jogador jogo)}

@

@
colideEscada :: Mapa -> Personagem -> Bool
colideEscada (Mapa _ _ blocos) p = intersecaoTotalListas (gethitbox p) (getHitboxEscada blocos )

@

@
getHitboxEscada :: [[Bloco]] -> [Hitbox]
getHitboxEscada blocos = map getHitboxBloco (tiraposicaobloco Escada blocos)

@
getHitboxEscadaMapa :: Mapa -> [Hitbox]
getHitboxEscadaMapa (Mapa _ _ blocos) = getHitboxEscada blocos
@

@
invertedirecoes :: Personagem -> Personagem
invertedirecoes p 
    | direcao p == Este = p {direcao = Oeste}
    | direcao p == Oeste = p {direcao = Este}
    | otherwise = p

@

-}

validaEmEscada:: Mapa -> Personagem -> Personagem
validaEmEscada mapa jogador | colisoesEscada mapa jogador = jogador{emEscada=True} 
                            | otherwise = jogador{emEscada=False}

escadasJogo :: Jogo -> Jogo
escadasJogo jogo = jogo {jogador = validaEmEscada (mapa jogo)(jogador jogo)}

colideEscada :: Mapa -> Personagem -> Bool
colideEscada (Mapa _ _ blocos) p = intersecaoTotalListas (gethitbox p) (getHitboxEscada blocos )

getHitboxEscada :: [[Bloco]] -> [Hitbox]
getHitboxEscada blocos = map getHitboxBloco (tiraposicaobloco Escada blocos)

getHitboxEscadaMapa :: Mapa -> [Hitbox]
getHitboxEscadaMapa (Mapa _ _ blocos) = getHitboxEscada blocos

invertedirecoes :: Personagem -> Personagem
invertedirecoes p 
    | direcao p == Este = p {direcao = Oeste}
    | direcao p == Oeste = p {direcao = Este}
    | otherwise = p

