-- Lukas Francisco Ferreira de Sá 202265568C
-- Enzo Faceroli Marques Moreira 202265566C

import Data.Char
import Data.Bits
import System.Random

estadoInicialJogo :: IO [Int]
estadoInicialJogo = do
    numFileiras <- randomRIO (2, 5) :: IO Int
    mapM (\_ -> randomRIO (1, 7) >>= \x -> return (if odd x then x else x + 1)) [1..numFileiras]

imprimirNovaLinha :: IO ()
imprimirNovaLinha = putChar '\n'

obterDigitoValido :: String -> IO Int
obterDigitoValido mensagem = do
    putStr mensagem
    entrada <- getLine
    if all isDigit entrada then
        return (read entrada :: Int)
    else do
        imprimirNovaLinha
        putStrLn "Entrada inválida. Por favor, insira um dígito."
        obterDigitoValido mensagem

obterSomaBinaria :: [Int] -> Int
obterSomaBinaria estadoJogo = foldl1 xor estadoJogo

encontrarMovimentoVencedor :: [Int] -> (Int, Int)
encontrarMovimentoVencedor estadoJogo = case [(linha, estrelas - alvo) | (estrelas, linha) <- zip estadoJogo [1..], 
                                                                        let alvo = estrelas `xor` obterSomaBinaria estadoJogo, 
                                                                        alvo < estrelas] of
    [] -> (1, 1)
    (movimento:_) -> movimento

obterMovimentoComputadorFacil :: [Int] -> IO (Int, Int)
obterMovimentoComputadorFacil estadoJogo = do
    let linhasComEstrelas = [i | (i, estrelas) <- zip [1..] estadoJogo, estrelas > 0]
    linha <- randomRIO (1, length linhasComEstrelas)
    let linhaSelecionada = linhasComEstrelas !! (linha - 1)
    qtdeEstrelas <- randomRIO (1, estadoJogo !! (linhaSelecionada - 1))
    return (linhaSelecionada, qtdeEstrelas)

obterMovimentoComputadorDificil :: [Int] -> IO (Int, Int)
obterMovimentoComputadorDificil estadoJogo = return (encontrarMovimentoVencedor estadoJogo)

exibirEstadoJogo :: [Int] -> IO ()
exibirEstadoJogo estadoJogo = 
    putStr $ unlines [show idx ++ ": " ++ replicate cont '*' | (cont, idx) <- zip estadoJogo [1..]]

jogoAcabou :: [Int] -> Bool
jogoAcabou estadoJogo = all (== 0) estadoJogo

movimentoValido :: [Int] -> Int -> Int -> Bool
movimentoValido estadoJogo linha qtdeEstrelas = 
    linha > 0 && linha <= length estadoJogo && (estadoJogo !! (linha - 1)) >= qtdeEstrelas

atualizarEstadoJogo :: [Int] -> Int -> Int -> [Int]
atualizarEstadoJogo estadoJogo linha qtdeEstrelas = 
    [if idx == linha then estrelas - qtdeEstrelas else estrelas | (estrelas, idx) <- zip estadoJogo [1..]]

loopJogo :: [Int] -> Bool -> Bool -> IO ()
loopJogo estadoJogo jogadorHumano nivelFacil = do
    exibirEstadoJogo estadoJogo
    if jogoAcabou estadoJogo then do
        if jogadorHumano then
            putStrLn "O computador venceu!"
        else
            putStrLn "Você venceu!"
    else if jogadorHumano then do
        putStrLn "Vez do jogador."
        linhaSelecionada <- obterDigitoValido "Selecione uma linha: "
        estrelasParaRemover <- obterDigitoValido "Insira o número de estrelas a remover: "
        if linhaSelecionada > 0 && linhaSelecionada <= length estadoJogo then
            if movimentoValido estadoJogo linhaSelecionada estrelasParaRemover then
                loopJogo (atualizarEstadoJogo estadoJogo linhaSelecionada estrelasParaRemover) False nivelFacil
            else do
                putStrLn "Movimento inválido. Tente novamente."
                loopJogo estadoJogo True nivelFacil
        else do
            putStrLn "Linha inválida. Tente novamente."
            loopJogo estadoJogo True nivelFacil
    else do
        putStrLn "Vez do computador."
        (linhaComp, estrelasComp) <- if nivelFacil 
                                     then obterMovimentoComputadorFacil estadoJogo 
                                     else obterMovimentoComputadorDificil estadoJogo
        let estrelasRemover = min estrelasComp (estadoJogo !! (linhaComp - 1))
        putStrLn $ "Computador remove " ++ show estrelasRemover ++ " estrelas da linha " ++ show linhaComp ++ "."
        loopJogo (atualizarEstadoJogo estadoJogo linhaComp estrelasRemover) True nivelFacil

escolherNivel :: IO Bool
escolherNivel = do
    putStrLn "Escolha o nível de dificuldade:"
    putStrLn "1. Fácil"
    putStrLn "2. Difícil"
    nivel <- obterDigitoValido "Digite o número do nível: "
    return (nivel == 1)

iniciarJogoPalitos :: IO ()
iniciarJogoPalitos = do
    nivelFacil <- escolherNivel
    estadoInicial <- estadoInicialJogo
    loopJogo estadoInicial True nivelFacil



