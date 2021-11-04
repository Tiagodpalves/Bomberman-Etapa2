module Main where
import System.IO
import Text.Read ( readMaybe )
import Data.Maybe ( fromMaybe, isNothing, fromJust )

main = do{moveloop primeiroMovimento} 



{-     **************** Bomberman - Etapa 1 ***************

Alunos : Tiago de Paula Alves (12011BCC040)
         Bruno Ferreira Tomé (12011BCC050)  



*********** Partes que na Teoria estao prontas, mesmo que precisem ser otimizadas:   ***********

- Criar o tabuleiro : cria as linhas e as linhas criam as celulas 

-Movimentação do Persongem  : pega um personagem (1 a 4) e pega as informações dele 
assim como a direção desejada verifica se ela é possivel (tabuleiro muda se tiver um presente no local
se for apenas grama nao muda) e caso seja realiza o movimento e coleta o presente
e incrementa sua posição nas capacidades, e é retornado um movimento que é uma tupla 
que contem o atual tabuleiro e Jogadores

-- Arremesso e Explosões -- 

-Esta funcional, ao colocar uma bomba o personagem tem uma janela de fuga, 
esta é relacionada a direção na qual ele vai apontar a partir de colocar a bomba, 
ele pula 2 quadrados, visto que a bomba em si te uma área de explosão em 5 lugares 
o local deixado e cada uma de suas adjacências.
-Ao encontrar uma bomba o jogador pode se posicionar em cima dela 
e arremessa-la a frente por também 2 posições, caso tenha a habilidade.

-- Etapa 2 --
-Tal etapa tem como intuito utilizar as funções já presentes na etapa 1 
e implementar um menu para interagir com o usuário e adaptar para a utilização de data e tipos algébricos.
-Foi criado dois data, direção e ação, para movimentar o personagem 
e o que ele ira fazer a seguir. O usuário insere qual jogador ele ira escolher
e qual sua ação, a cada loop aparece como esta o cenário e as informações do jogador, 
a ação continua até inserir a opção E (Exit). 



-}
-- a = Patins, b - Bomba, c = Arremesso
type Capacidades = (Int, Int, Int)
type Jogadores = (Jogador_X, Jogador_X, Jogador_X, Jogador_X)
type Jogador_X = (Int, Int, Int, Capacidades) -- (qual_jogador, coordenada x, coordenada y, capacidades) 
type Item = [Char] 
type Celula = (Int, Int, Item)
type Linha = (Celula, Celula, Celula, Celula, Celula, Celula, Celula, Celula)
type Tabuleiro = (Linha, Linha, Linha, Linha, Linha, Linha, Linha, Linha) 
type Move = (Tabuleiro, Jogadores) 

-- ********* Parte do Menu **********

data Direcao = Norte | Sul | Leste | Oeste deriving (Show, Eq)
data Acao = ColocarBomba Direcao | Agir Direcao | Mover Direcao | NO_OP | Sair deriving (Show, Eq)


moveloop :: Move -> IO()
moveloop (tab, j) = 
          do
            print (itemSuperficie (tab, j))
            n <- jogadorVez
            print(infoJogadores n j)
            op <- menu
            if op == Sair then return()
            else let (tab', j') = case op of Mover d -> movimenta n d (tab, j) 
                                             ColocarBomba d -> colocaBomba (tab, j) n d
                                             Agir d -> jogaBomba (tab, j) n d
                                             NO_OP -> (tab, j) 
                                             Sair -> (tab, j)
                 in moveloop (tab', j')


menu :: IO Acao  
menu = do 
        putStrLn "O que voce deseja fazer? \n->Se movimentar (N, S, L, O) \n->Colocar Bomba e sua direcao (BN, BS, BL, BO) \n->Arremessar uma bomba e sua direcao (AN, AS, AL, AO)\n->Sair (E)"
        opcao <- getLine 
        return(case opcao of "N" -> Mover Norte 
                             "S" -> Mover Sul 
                             "L" -> Mover Leste 
                             "O" -> Mover Oeste
                             "BN" -> ColocarBomba Norte
                             "BS" -> ColocarBomba Sul
                             "BL" -> ColocarBomba Leste
                             "BO" -> ColocarBomba Oeste
                             "AN" -> Agir Norte
                             "AS" -> Agir Sul
                             "AL" -> Agir Leste
                             "AO" -> Agir Oeste
                             "E" -> Sair
                             _ -> NO_OP )


jogadorVez :: IO Int
jogadorVez = do
            putStrLn "\nQual o jogador da vez : "
            n <- getLine
            return (read n)


-- ********* Parte de Movimentacao **********

-- Cria o mapa e os jogadores
primeiroMovimento :: Move
primeiroMovimento = (gerarTabuleiro, (introJogadores (gerarTabuleiro)))

-- Movimenta o persoagem, recebe qual o jogador, a direção que deseja ir e um movimento ja existente (Tabuleiro, Jogadores)
movimenta :: Int -> Direcao -> Move -> Move
movimenta qJ d (tab, j) = 
  let jogador = infoJogadores qJ j 
      (x1,y1) = direcao jogador d 
      c = possivelMovimento (x1,y1) tab
      nJ = adicionarCapacidade qJ (x1,y1) c j
      nTab = mudarTabuleiro (x1,y1) c tab

  in 
      (nTab, nJ)
      

direcao :: Jogador_X -> Direcao -> (Int,Int)
direcao (iD,x,y,cap) c  | (c == Norte) && x+1 <= 7 =  (x+1, y)  
                        | (c == Sul) && x-1 >= 0 =  (x-1, y) 
                        | (c == Leste) && y+1 <= 7 =  (x, y+1) 
                        | (c == Oeste) && y-1 >= 0 =  (x, y-1) 
                        | otherwise = error "A coordenada desejada nao é acessivel"

direcaoDobro :: Jogador_X -> Direcao -> (Int,Int)
direcaoDobro (iD,x,y,cap) c | (c == Norte) && x+2 <= 7 =  (x+2, y)  
                            | (c == Sul) && x-2 >= 0 =  (x-2, y) 
                            | (c == Leste) && y+2 <= 7 =  (x, y+2) 
                            | (c == Oeste) && y-2 >= 0 =  (x, y-2) 
                            | otherwise = error "A coordenada desejada nao é acessivel"

-- verifica se mov possivel recebe a coordenada e o tabuleiro 

possivelMovimento :: (Int,Int) -> Tabuleiro -> Char
possivelMovimento (x,y) (l0,l1,l2,l3,l4,l5,l6,l7)   | x == 0 = (verificaLinha y l0) 
                                                    | x == 1 = (verificaLinha y l1)   
                                                    | x == 2 = (verificaLinha y l2)   
                                                    | x == 3 = (verificaLinha y l3)   
                                                    | x == 4 = (verificaLinha y l4)   
                                                    | x == 5 = (verificaLinha y l5)    
                                                    | x == 6 = (verificaLinha y l6)    
                                                    | x == 7 = (verificaLinha y l7) 

verificaLinha :: Int -> Linha -> Char
verificaLinha y (c0,c1,c2,c3,c4,c5,c6,c7) | y == 0 = (verificaCelula c0) 
                                          | y == 1 = (verificaCelula c1)   
                                          | y == 2 = (verificaCelula c2)  
                                          | y == 3 = (verificaCelula c3) 
                                          | y == 4 = (verificaCelula c4)   
                                          | y == 5 = (verificaCelula c5)  
                                          | y == 6 = (verificaCelula c6)  
                                          | y == 7 = (verificaCelula c7) 

verificaCelula :: Celula -> Char
verificaCelula (_,_,[]) = error "Error. Lista vazia"
verificaCelula (_,_,[x]) = if x == 'g' then 'g'
                           else 'N'
verificaCelula (_,_,(x:y)) =  if x == 'g' && head y == 'a' then 'a'
                              else if x == 'g' && head y == 'b' then 'b'
                              else if x == 'g' && head y == 'c' then 'c'
                              else 'N'
                              




-- ************ Crição do Mapa ***************

gerarTabuleiro :: Tabuleiro  
gerarTabuleiro = (linha0,linha1,linha2,linha3,linha4,linha5,linha6,linha7)
                  where linha0 = gerarLinha 0   
                        linha1 = gerarLinha 1  
                        linha2 = gerarLinha 2  
                        linha3 = gerarLinha 3  
                        linha4 = gerarLinha 4  
                        linha5 = gerarLinha 5  
                        linha6 = gerarLinha 6  
                        linha7 = gerarLinha 7  

gerarLinha :: Int -> Linha  
gerarLinha x = (celula0, celula1, celula2, celula3, celula4, celula5, celula6, celula7)
                where celula0 = gerarItens x 0 
                      celula1 = gerarItens x 1 
                      celula2 = gerarItens x 2 
                      celula3 = gerarItens x 3 
                      celula4 = gerarItens x 4 
                      celula5 = gerarItens x 5 
                      celula6 = gerarItens x 6 
                      celula7 = gerarItens x 7 

-- grama('g') -> Capacidaes('a','b','c') 
-- grama -> jogador_X ('1', '2', '3', '4')
-- grama -> parede('p')
-- buraco ('b')
-- pedra('#')

gerarItens :: Int -> Int -> Celula
gerarItens x y  | x == 3 && y == 4 = (x,y,['#'])
                | x < 8 && y == 0 = (x,y,['#'])
                | x == 0 && y < 8 = (x,y,['#'])
                | x < 8 && y == 7 = (x,y,['#'])
                | x == 7 && y < 8 = (x,y,['#'])
                | x == 6 && y == 3 = (x,y,['g','a'])
                | x == 2 && y == 6 = (x,y,['g','a'])
                | x == 5 && y == 4 = (x,y,['g','b'])
                | x == 2 && y == 3 = (x,y,['g','b'])
                | x == 6 && y == 2 = (x,y,['g','b'])
                | x == 3 && y == 6 = (x,y,['g','c'])
                | x == 3 && y == 3 = (x,y,['g','c'])
               -- | x == 5 && y == 5 = (x,y,['g','v'])
               -- | x == 4 && y == 3 = (x,y,['g','v'])
                | otherwise = (x,y,['g'])

-- verifica se o mapa esta na ordem certa (nao foi necessario seu uso por enquanto)
prioridadeCelula :: Celula -> Bool
prioridadeCelula (x,y,c)  | cabeça == 'g' && (head cauda == 'a' ||  head cauda == 'b' || head cauda == 'c') = True
                          | cabeça == 'g' && (head cauda == '1' ||  head cauda == '2' || head cauda == '3' || head cauda == '4') = True
                          | cabeça == 'g' && (head cauda == 'p' &&  tail cauda == []) = True
                          | cabeça == '#' && cauda == [] = True
                          | otherwise = False
                          where cabeça = head c
                                cauda = tail c


-- ************ Crição dos Jogadores ***************

-- Introduz os jogadoes no jogo com uma movimentação diferenciada, para que crie o mapa uma menor quantidade de vezes

introJogadores :: Tabuleiro -> Jogadores
introJogadores (linha0,linha1,linha2,linha3,linha4,linha5,linha6,linha7)  = (jogador1, jogador2, jogador3, jogador4)
                                      where
                                       jogador1 = localDisponivel 1 (0,0,0) linha1 
                                       jogador2 = localDisponivel 2 (0,0,0) linha3 
                                       jogador3 = localDisponivel 3 (0,0,0) linha5 
                                       jogador4 = localDisponivel 4 (0,0,0) linha6 

localDisponivel :: Int -> Capacidades -> Linha -> Jogador_X                  
localDisponivel z cap ((x0, y0, c0), (x1, y1, c1), (x2, y2, c2), (x3, y3, c3), (x4, y4, c4), (x5, y5, c5), (x6, y6, c6), (x7, y7, c7))
                                                | celulaDisponivel c0 == True = (z,x0,x0,cap)  
                                                | celulaDisponivel c1 == True = (z,x1,y1,cap) 
                                                | celulaDisponivel c2 == True = (z,x2,y2,cap) 
                                                | celulaDisponivel c3 == True = (z,x3,y3,cap) 
                                                | celulaDisponivel c4 == True = (z,x4,y4,cap) 
                                                | celulaDisponivel c5 == True = (z,x5,y5,cap) 
                                                | celulaDisponivel c6 == True = (z,x6,y6,cap) 
                                                | celulaDisponivel c7 == True = (z,x7,y7,cap) 

celulaDisponivel :: Item -> Bool
celulaDisponivel [] = error "Error. Lista vazia"
celulaDisponivel [x] = if x == 'g' then True
                           else False


-- ************ Auxiliares: ***************

infoJogadores :: Int -> Jogadores -> Jogador_X
infoJogadores j (j1,j2,j3,j4) | j == 1 = j1 
                              | j == 2 = j2   
                              | j == 3 = j3  
                              | j == 4 = j4  


-- ************ Refazer o Mapa  ***************

mudarTabuleiro :: (Int,Int) -> Char -> Tabuleiro -> Tabuleiro
mudarTabuleiro (x,y) c (l0,l1,l2,l3,l4,l5,l6,l7)
                                              | c == 'g' = (l0,l1,l2,l3,l4,l5,l6,l7)
                                              | c == 'N' = error "Nao eh possivel mover"
                                              | x == 0 = (linha0,l1,l2,l3,l4,l5,l6,l7)
                                              | x == 1 = (l0,linha1,l2,l3,l4,l5,l6,l7)
                                              | x == 2 = (l0,l1,linha2,l3,l4,l5,l6,l7)
                                              | x == 3 = (l0,l1,l2,linha3,l4,l5,l6,l7)
                                              | x == 4 = (l0,l1,l2,l3,linha4,l5,l6,l7)
                                              | x == 5 = (l0,l1,l2,l3,l4,linha5,l6,l7)
                                              | x == 6 = (l0,l1,l2,l3,l4,l5,linha6,l7)
                                              | x == 7 = (l0,l1,l2,l3,l4,l5,l6,linha7)
                                              where linha0 = mudarLinha (x,y) l0
                                                    linha1 = mudarLinha (x,y) l1
                                                    linha2 = mudarLinha (x,y) l2
                                                    linha3 = mudarLinha (x,y) l3
                                                    linha4 = mudarLinha (x,y) l4
                                                    linha5 = mudarLinha (x,y) l5
                                                    linha6 = mudarLinha (x,y) l6
                                                    linha7 = mudarLinha (x,y) l7

mudarLinha :: (Int,Int) ->  Linha -> Linha
mudarLinha (x,y) (c0,c1,c2,c3,c4,c5,c6,c7)  | y == 0 = (celula0,c1,c2,c3,c4,c5,c6,c7)  
                                            | y == 1 = (c0,celula1,c2,c3,c4,c5,c6,c7) 
                                            | y == 2 = (c0,c1,celula2,c3,c4,c5,c6,c7) 
                                            | y == 3 = (c0,c1,c2,celula3,c4,c5,c6,c7)   
                                            | y == 4 = (c0,c1,c2,c3,celula4,c5,c6,c7)  
                                            | y == 5 = (c0,c1,c2,c3,c4,celula5,c6,c7)   
                                            | y == 6 = (c0,c1,c2,c3,c4,c5,celula6,c7) 
                                            | y == 7 = (c0,c1,c2,c3,c4,c5,c6,celula7)
                                            where celula0 = mudaCelula (x,y) 
                                                  celula1 = mudaCelula (x,y) 
                                                  celula2 = mudaCelula (x,y) 
                                                  celula3 = mudaCelula (x,y) 
                                                  celula4 = mudaCelula (x,y) 
                                                  celula5 = mudaCelula (x,y) 
                                                  celula6 = mudaCelula (x,y) 
                                                  celula7 = mudaCelula (x,y) 

mudaCelula :: (Int, Int) -> Celula
mudaCelula (x,y) = (x,y,['g'])


-- ************ Refazer o jogador  ***************

adicionarCapacidade :: Int -> (Int,Int) -> Char -> Jogadores -> Jogadores
adicionarCapacidade qj (x,y) c (jogador1, jogador2, jogador3, jogador4) 
                                      | c == 'a' && qj == 1 = ((qj,x,y,(1,0,0)), jogador2, jogador3, jogador4) 
                                      | c == 'c' && qj == 1 = ((qj,x,y,(0,0,1)), jogador2, jogador3, jogador4)
                                      | c == 'N' && qj == 1 = ((qj,x,y,(0,0,0)), jogador2, jogador3, jogador4) 
                                      | c == 'a' && qj == 2 = (jogador1, (qj,x,y,(1,0,0)), jogador3, jogador4)
                                      | c == 'c' && qj == 2 = (jogador1, (qj,x,y,(0,0,1)), jogador3, jogador4)
                                      | c == 'N' && qj == 2 = (jogador1, (qj,x,y,(0,0,0)), jogador3, jogador4)
                                      | c == 'a' && qj == 3 = (jogador1, jogador2, (qj,x,y,(1,0,0)), jogador4)
                                      | c == 'c' && qj == 3 = (jogador1, jogador2, (qj,x,y,(0,0,1)), jogador4)
                                      | c == 'N' && qj == 3 = (jogador1, jogador2, (qj,x,y,(0,0,0)), jogador4)
                                      | c == 'a' && qj == 4 = (jogador1, jogador2,jogador3 , (qj,x,y,(1,0,0)))
                                      | c == 'c' && qj == 4 = (jogador1, jogador2, jogador3, (qj,x,y,(0,0,1)))
                                      | c == 'N' && qj == 4 = (jogador1, jogador2, jogador3, (qj,x,y,(0,0,0)))
                                      | otherwise = (moverJogador qj (x,y) (jogador1, jogador2, jogador3, jogador4))
          
moverJogador :: Int -> (Int, Int) -> Jogadores -> Jogadores
moverJogador qj (xN,yN) ((qj1, x0, y0, c0), (qj2, x1, y1, c1), (qj3, x2, y2, c2), (qj4, x3, y3, c3))
                                      | qj == 1 = ((qj1, xN, yN, c0), (qj2, x1, y1, c1), (qj3, x2, y2, c2), (qj4, x3, y3, c3))
                                      | qj == 2 = ((qj1, x0, y0, c0), (qj2, xN, yN, c1), (qj3, x2, y2, c2), (qj4, x3, y3, c3))
                                      | qj == 3 = ((qj1, x0, y0, c0), (qj2, x1, y1, c1), (qj3, xN, yN, c2), (qj4, x3, y3, c3))
                                      | qj == 4 = ((qj1, x0, y0, c0), (qj2, x1, y1, c1), (qj3, x2, y2, c2), (qj4, xN, yN, c3))

-- ************ Arremesso ***************

verificaArremesso :: Tabuleiro -> Jogador_X -> Bool
verificaArremesso tab (qj,x,y,c)  | c == (1,0,1) && local == 'b' = True
                                  | c == (0,0,1) && local == 'b' = True
                                  | otherwise = False
                                    where local = possivelMovimento (x,y) tab 


mudaArremesso :: Jogador_X -> Jogadores -> Jogadores
mudaArremesso (qj,x,y,c) jogadores | c == (1,0,1) = adicionarCapacidade qj (x,y) 'a' jogadores
                                   | c == (0,0,1) = adicionarCapacidade qj (x,y) 'N' jogadores
                                   | otherwise = jogadores


jogaBomba :: Move -> Int -> Direcao -> Move 
jogaBomba (tab, jogadores) n c  | havepower == True && possivelDestino /= '#' && n == 1 = (realizaExplosao destino (tab, nj))
                                | havepower == True && possivelDestino /= '#' && n == 2 = (realizaExplosao destino (tab, nj))
                                | havepower == True && possivelDestino /= '#' && n == 3 = (realizaExplosao destino (tab, nj))
                                | havepower == True && possivelDestino /= '#' && n == 4 = (realizaExplosao destino (tab, nj))
                                | otherwise = (tab, jogadores) 
                                where jogador = (infoJogadores n jogadores)
                                      havepower = (verificaArremesso tab jogador)
                                      destino = (direcaoDobro jogador c)
                                      possivelDestino = (possivelMovimento destino tab)
                                      nj = mudaArremesso jogador jogadores

-- Da uma chance para ele fugir pulando 2 casas para uma das direções 
colocaBomba :: Move -> Int -> Direcao -> Move 
colocaBomba (tab, (j1,j2,j3,j4)) n c | n == 1 = (realizaExplosao (x,y) (tab, (nj1,j2,j3,j4)))
                                     | n == 2 = (realizaExplosao (x,y) (tab, (j1,nj2,j3,j4)))
                                     | n == 3 = (realizaExplosao (x,y) (tab, (nj1,j2,nj3,j4)))
                                     | n == 4 = (realizaExplosao (x,y) (tab, (nj1,j2,j3,nj4)))
                                     where (qj,x,y,cap) = (infoJogadores n (j1,j2,j3,j4)) 
                                           nj1 = foge j1 c tab
                                           nj2 = foge j2 c tab
                                           nj3 = foge j3 c tab
                                           nj4 = foge j4 c tab

foge :: Jogador_X -> Direcao -> Tabuleiro -> Jogador_X
foge (qj,x,y,cap) c tab = if l1 == 'g' && l2 == 'g' then (qj,xN2,yN2,cap)
                          else if l1 == 'g' && l2 /= 'g' then (qj,xN,yN,cap)
                          else (qj,x,y,cap) 
                          where (xN,yN) = (direcao (qj,x,y,cap) c) 
                                (xN2,yN2) = (direcaoDobro (qj,x,y,cap) c)
                                l1 = possivelMovimento (xN,yN) tab 
                                l2 = possivelMovimento (xN2,yN2) tab
    
-- ************ Explosao ***************

realizaExplosao :: (Int,Int) -> Move -> Move
realizaExplosao (x,y) (tab,jogadores) = (novoTab, novosJogadores)
                                        where novoTab = explosaoTab tab (x,y)
                                              novosJogadores = explosaoJogador (x,y) jogadores 


explosaoTab :: Tabuleiro -> (Int,Int) -> Tabuleiro 
explosaoTab (l0,l1,l2,l3,l4,l5,l6,l7) (x,y)
                                          | x == 0 = (linha0,linha1,l2,l3,l4,l5,l6,l7)
                                          | x == 1 = (linha0,linha1,linha2,l3,l4,l5,l6,l7)
                                          | x == 2 = (l0,linha1,linha2,linha3,l4,l5,l6,l7)
                                          | x == 3 = (l0,l1,linha2,linha3,linha4,l5,l6,l7)
                                          | x == 4 = (l0,l1,l2,linha3,linha4,linha5,l6,l7)
                                          | x == 5 = (l0,l1,l2,l3,linha4,linha5,linha6,l7)
                                          | x == 6 = (l0,l1,l2,l3,l4,linha5,linha6,linha7)
                                          | x == 7 = (l0,l1,l2,l3,l4,l5,linha6,linha7)
                                          where linha0 = explosaoLinha (x,y) l0
                                                linha1 = explosaoLinha (x,y) l1
                                                linha2 = explosaoLinha (x,y) l2
                                                linha3 = explosaoLinha (x,y) l3
                                                linha4 = explosaoLinha (x,y) l4
                                                linha5 = explosaoLinha (x,y) l5
                                                linha6 = explosaoLinha (x,y) l6
                                                linha7 = explosaoLinha (x,y) l7

explosaoLinha :: (Int, Int) -> Linha -> Linha
explosaoLinha (x,y) (c0,c1,c2,c3,c4,c5,c6,c7) | y == 0 = (celula0,celula1,c2,c3,c4,c5,c6,c7)  
                                              | y == 1 = (celula0,celula1,celula2,c3,c4,c5,c6,c7) 
                                              | y == 2 = (c0,celula1,celula2,celula3,c4,c5,c6,c7) 
                                              | y == 3 = (c0,c1,celula2,celula3,celula4,c5,c6,c7)   
                                              | y == 4 = (c0,c1,c2,celula3,celula4,celula5,c6,c7)  
                                              | y == 5 = (c0,c1,c2,c3,celula4,celula5,celula6,c7)   
                                              | y == 6 = (c0,c1,c2,c3,c4,celula5,celula6,celula7) 
                                              | y == 7 = (c0,c1,c2,c3,c4,c5,celula6,celula7)
                                              where celula0 = explosaoCelula c0 
                                                    celula1 = explosaoCelula c1
                                                    celula2 = explosaoCelula c2
                                                    celula3 = explosaoCelula c3 
                                                    celula4 = explosaoCelula c4
                                                    celula5 = explosaoCelula c5
                                                    celula6 = explosaoCelula c6
                                                    celula7 = explosaoCelula c7 

explosaoCelula :: Celula -> Celula
explosaoCelula (x1,y1,(x:xs)) = if x == '#' then (x1,y1,(x:xs))
                                else (x1,y1,['g'])


explosaoJogador :: (Int,Int) -> Jogadores -> Jogadores 
explosaoJogador (x,y) (j1,j2,j3,j4) = (gameOver1,gameOver2,gameOver3,gameOver4)
            where gameOver1 = verJogador (x,y) j1 
                  gameOver2 = verJogador (x,y) j2 
                  gameOver3 = verJogador (x,y) j3
                  gameOver4 = verJogador (x,y) j4

verJogador :: (Int,Int) -> Jogador_X -> Jogador_X 
verJogador (x,y) (qj1,x1,y1,c1) = if (x1 == x && y1 == y) || (x1 == x+1 && y1 == y) || (x1 == x-1 && y1 == y) || (x1 == x && y1 == y+1) || (x1 == x && y1 == y-1) then (-1,-1,-1,c1)
                                  else (qj1,x1,y1,c1)
                                  
-- imprimir
imprimirLista ::  [[a]] -> [a]
imprimirLista (x:[]) = x
imprimirLista (x:xs) = x ++ imprimirLista xs

itemSuperficie :: Move -> [[[Char]]]
itemSuperficie ((l0,l1,l2,l3,l4,l5,l6,l7), jogadores) = [linha0, linha1, linha2, linha3, linha4, linha5, linha6, linha7]     
                                                      where linha0 = itemLinha l0 0 jogadores
                                                            linha1 = itemLinha l1 1 jogadores
                                                            linha2 = itemLinha l2 2 jogadores
                                                            linha3 = itemLinha l3 3 jogadores
                                                            linha4 = itemLinha l4 4 jogadores
                                                            linha5 = itemLinha l5 5 jogadores
                                                            linha6 = itemLinha l6 6 jogadores
                                                            linha7 = itemLinha l7 7 jogadores

itemLinha :: Linha -> Int -> Jogadores -> [[Char]]
itemLinha (c0,c1,c2,c3,c4,c5,c6,c7) x jogadores = [celula0,celula1,celula2,celula3,celula4,celula5,celula6,celula7]
                                                  where celula0 = itemcelula c0 x 0 jogadores
                                                        celula1 = itemcelula c1 x 1 jogadores
                                                        celula2 = itemcelula c2 x 2 jogadores
                                                        celula3 = itemcelula c3 x 3 jogadores
                                                        celula4 = itemcelula c4 x 4 jogadores
                                                        celula5 = itemcelula c5 x 5 jogadores
                                                        celula6 = itemcelula c6 x 6 jogadores
                                                        celula7 = itemcelula c7 x 7 jogadores

itemcelula :: Celula -> Int -> Int -> Jogadores -> [Char]
itemcelula (_,_,[]) x y jogadores = ['N']
itemcelula (_,_,_) x y jogadores  | x == x1 && y == y1 = ['1']
                                  | x == x2 && y == y2 = ['2']
                                  | x == x3 && y == y3 = ['3']
                                  | x == x4 && y == y4 = ['4']
                                  where (qj1,x1,y1,c1) = infoJogadores 1 jogadores
                                        (qj2,x2,y2,c2) = infoJogadores 2 jogadores
                                        (qj3,x3,y3,c3) = infoJogadores 3 jogadores
                                        (qj4,x4,y4,c4) = infoJogadores 4 jogadores
itemcelula (_,_,[s]) x y jogadores = [s]
itemcelula (_,_,(s:sy)) x y jogadores  = [head sy]
                            
                              
