# Operações seguras com matrizes e programação no nível de tipos em Haskell

Matrizes são estruturas de dados muito utilizadas em computação científica. No entanto, lidar com essas estruturas pode ser complicado, pois sempre temos que ter em mente as dimensões de cada estrutura na hora de realizar operações com elas.

Para facilitar a criação de programas que lidam com matrizes, podemos utilizar as capacidades de programação no nível de tipos da linguagem Haskell. Neste texto, vamos explorar como podemos criar estruturas de dados com garantias no nível dos tipos e como podemos aumentar ainda mais essa segurança para lidar com dimensões fornecidas em tempo de execução.

## Lista indexável

Antes de falarmos propriamente sobre matrizes, vamos falar sobre listas indexáveis. Uma lista indexável é uma lista no qual podemos acessar qualquer um de seus elementos através de um número inteiro, que representa a posição do elemento na lista. Em Haskell, já possuímos uma estrutura de dados linear indexável:

```hs
data [a] = [] | a : [a]
```

cuja operação de indexação é feita através da função `!!`:

```hs
(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n-1)
```

Aparentemente, tudo funciona como esperado. No entanto, essa definição de lista não é nada segura, pois podemos tentar acessar um índice que não existe na lista, o que resulta em um erro em tempo de execução. O mesmo ocorre para outras funções da lista, como `head` e `tail` que podem falhar se a lista estiver vazia.

Usando programação no nível dos tipos, podemos tornar a lista indexável mais segura. Para isso, vamos definir um novo tipo de lista:

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

import Data.Kind

type ArrayN :: Nat -> Type -> Type
data ArrayN n a where
  Nil :: ArrayN Z a
  (:>) :: a -> ArrayN n a -> ArrayN (S n) a

infixr 5 :>
```

Essa implementação depende ainda de uma definição de números naturais no nível dos tipos, que é usada para representar, em tempo de compilação, o tamanho da lista. Para isso, vamos definir o tipo `Nat` usando números de Peano:

```hs
data Nat = Z | S Nat

-- Z <-> 0
-- S Z <-> 1
-- S (S Z) <-> 2
-- ...
```

Agora, podemos definir funções seguras para acessar a cabeça e a cauda da lista por exemplo:

```hs
lhead :: ArrayN (S n) a -> a
lhead (x :> _) = x

ltail :: ArrayN (S n) a -> ArrayN n a
ltail (_ :> xs) = xs
```

Nessas definições, a própria assinatura da função delimita quais listas podem ser passadas como argumento. As duas funções só aceitam listas com pelos menos um elemento, o que garante, em tempo de compilação, que nunca tentaremos acessar a cabeça ou a cauda de uma lista vazia.

Como essa leve introdução ao mundo da programação no nível de tipos feita, podemos partir para implementação de tipos mais complexos, como matrizes.

## Matrizes

Uma matriz é uma estrutura de dados bidimensional, ou seja, toda matriz possui uma quantidade de linhas e uma quantidade de colunas, ambas representadas por números inteiros. Assim, podemos utilizar a seguinte definição para representar uma matriz em Haskell:

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

import Data.Kind
import GHC.TypeNats

type Matrix :: Nat -> Nat -> Type -> Type
data Matrix n m a where
  M :: [[a]] -> Matrix n m a
```

Perceba que, assim como no caso da lista, a informação sobre as dimensões da matriz existe apenas no mundo dos tipos. Isso significa que, em tempo de compilação, o compilador pode decidir se uma operação com matrizes é válida ou não.

Diferentemente da implementação da lista, aqui estamos usando o tipo `Nat` definido pelo próprio GHC, as duas representação são equivalentes, com algumas diferenças sutis que não são importantes no momento.

Vamos criar funções para somar, subtrair, escalar e transpor matrizes:

```hs
addM :: (Num a) => Matrix r c a -> Matrix r c a -> Matrix r c a
addM (M xs) (M ys) = M $ zipWith (zipWith (+)) xs ys

(#+#) :: (Num a) => Matrix r c a -> Matrix r c a -> Matrix r c a
(#+#) = addM

scaleM :: (Num a) => a -> Matrix r c a -> Matrix r c a
scaleM x (M xs) = M $ map (map (x *)) xs

(*#) :: (Num a) => a -> Matrix r c a -> Matrix r c a
(*#) = scaleM

subM :: (Num a) => Matrix r c a -> Matrix r c a -> Matrix r c a
subM m n = addM m (scaleM (-1) n)

(#-#) :: (Num a) => Matrix r c a -> Matrix r c a -> Matrix r c a
(#-#) = subM

transposeM :: Matrix r c a -> Matrix c r a
transposeM (M xs) = M $ foldr (zipWith (:)) (repeat []) xs

(\^/) :: Matrix r c a -> Matrix c r a
(\^/) = transposeM

mulM :: (Num a) => Matrix r c a -> Matrix c c' a -> Matrix r c' a
mulM (M xs) ys = M $ map (\row -> map (sum . zipWith (*) row) ys') xs
  where
    (M ys') = transposeM ys

(#*#) :: (Num a) => Matrix r c a -> Matrix c c' a -> Matrix r c' a
(#*#) = mulM
```

Em todas as funções, as operações são válidas no nível dos tipos, ou seja, é impossível, por exemplo, somarmos matrizes de dimensões diferentes, pois o compilador não permitirá a compilação do programa caso tentemos usar a função `addM` com matrizes de dimensões diferentes.

Para entender ainda melhor isso, vamos pegar o exemplo da função de multiplicação de matrizes. A multiplicação de matrizes só é válida se o número de colunas da primeira matriz for igual ao número de linhas da segunda matriz. Assim, o compilador pode garantir que a multiplicação de matrizes só será feita se as dimensões das matrizes forem compatíveis. Isso é codificado na assinatura da função `mulM`:

```hs
mulM :: (Num a) => Matrix r c a -> Matrix c c' a -> Matrix r c' a
-- r -> número de linhas da primeira matriz
-- c -> número de colunas da primeira matriz
-- c' -> número de colunas da segunda matriz
```

Perceba que o número de linhas da primeira matriz é representado pelo mesmo tipo do número de colunas da primeira matriz `c`. O mesmo vale para matriz resultante.

## Aplicações práticas

### Sistemas lineares

### Processamento de imagens
<!-- Aplicações de Kernels de diferentes tamanhos e operações de convolução type-safe -->
