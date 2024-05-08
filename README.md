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

Diferentemente da implementação da lista, aqui estamos usando o tipo `Nat` definido pelo próprio GHC, as duas representação são equivalentes, com algumas diferenças que não são importantes no momento.

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

Agora que já temos as operações básicas de matrizes implementadas (com exceção da determinante e da inversa), podemos partir para a implementação de uma operação de indexação segura, ou seja, uma indexação que, em tempo de compilação, verifica se os valores passados são posições válidas no array.

## Indexação segura

Vamos começar pensando em como a indexação funciona em uma lista. Podemos indexar apenas uma lista que possui pelo menos um elemento, ou seja, cuja dimensão é maior ou igual a um. O mesmo vale para matrizes, mas precisamos pensar em duas dimensões. Além disso, sabemos que se podemos indexar uma lista na posição `n + 1`, podemos também indexar essa mesma lista na posição `n`, ou na posição `n - 1`, ou na posição `n - 2`, ou seja, em qualquer posição menor que `n + 1`. Isso é verdade para qualquer número inteiro `n`.

Se isso te faz lembrar do princípio da indução matemática, você está correto. Podemos usar a indução para criar um tipo que represente qualquer valor indexável para uma lista de tamanho `n`. Vamos explorar isso no seguinte tipo:

```hs
type Index :: Nat -> Type
data Index n where
  IZ :: Index (n + 1)
  IS :: Index n -> Index (n + 1)
```

Esse tipo codifica as regras de indexação que falamos no parágrafo anterior. O constritor `IZ` representa o índice do primeiro elemento da lista, enquanto o construtor `IS` representa o índice de um elemento qualquer cuja posição seja menor que `n`. No entanto, logo perceberemos que esse tipo não é nada prático de se usar, pois precisamos criar o índice que queremos encadeando sequências de `IS` até chegar no índice desejado.

Como podemos melhorar esse tipo sem perder a segurança que ele nos dá? Vamos pensar um pouco no nosso problema. Queremos que o valor do índice que desejamos (um valor em tempo de execução) seja menor que o tamanho da lista (um valor em tempo de compilação). Em outras palavras, queremos que um valor seja menor que um outro valor representado por um tipo. Temos uma dependência entre valores em tempo de execução e valores em tempo de compilação!

Como resolvemos esse problema? Em tempo de execução, não conseguimos saber qual o tipo que representa o tamanho da lista, já que ele não existe mais nesse momento, pois todo programa Haskell passa por um processo chamada **type erasure**, onde todos os tipos são removidos do programa. No entanto, podemos usar alguns truques para contornar esse questão.

Vamos considerar que para criar um índice válido, precisamos passar um valor número para uma função `idx`:

```hs
idx :: Int -> Index n
idx 0 = IZ
idx n = IS (idx (n - 1))
```

<!-- Criação de matrizes a partir de valores numéricos fornecidos pelos usuários -->

## Aplicações práticas

### Redes

Uma das aplicações mais importantes de matrizes é a representação de redes. Redes são estruturas de dados que representam conexões entre diferentes entidades. Para nosso exemplo, vamos considerar uma rede de computadores, onde cada nó é um computador e cada aresta é uma conexão entre dois computadores. Além disso, cada aresta possui um número entre 0 e 1 associado, que representa a qualidade da conexão entre os dois computadores.

Nessa rede, cada computador é uma função (não necessariamente diferente) que recebe várias entradas e produz uma única saída. Assim, podemos representar essa rede como uma matriz, onde cada linha representa um computador e cada coluna representa uma conexão entre dois computadores. O valor da matriz na posição `(i, j)` é a qualidade da conexão entre o computador `i` e o computador `j`.

Será que conseguimos criar uma estrutura de dados que represente essa rede e que garanta, em tempo de compilação, que todas as operações feitas com essa rede são válidas? Além disso, será que conseguimos nessa mesma estrutura, criar uma forma de acompanhar todas as operações realizadas em cada camada da rede?

### Sistemas lineares

Outra aplicação importante de matrizes é a resolução de sistemas lineares. Um sistema linear é um conjunto de equações lineares que possuem um número finito de variáveis. Podemos representar qualquer sistema linear como uma matriz, onde cada linha representa uma equação e cada coluna representa uma variável. Dessa forma, cada valor `(i, j)` da matriz representa o coeficiente da variável `j` na equação `i`.

Assim como no caso das redes, será que podemos criar uma estrutura com segurança no nível dos tipos que represente um sistema linear e que garanta a viabilidade das operações que realizarmos nele?

## Colocando a mão no código

A resposta para todas essas perguntas é sim e é isso que vamos criar agora. Já temos a base necessária para criar essas estruturas de dados e garantir a segurança no nível dos tipos. Inclusive, podemos representar ambas as estruturas da mesma forma, pois os problemas que descrevemos são equivalentes.

Considere uma rede de 3 camadas, onde a primeira camada possui 2 computadores, a segunda camada possui 3 computadores e a terceira camada possui 1 computador. Além disso, considere que a qualidade da conexão entre os computadores é dada pela seguinte matriz:

<!-- TODO: desenhar essa rede de computadores e entender como ela representa o mesmo problema do sistema linear -->

```hs
```

<!-- TODO: no final seria bom mostrar que essa representação de redes pode ser útil para outros fins, como calcular TODO: o número de possíveis caminhos entre um nó e outro em tempo de compilação -->
