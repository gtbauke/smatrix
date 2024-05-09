{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Matrix
  ( Nat (..),
    Matrix,
    addM,
    subM,
    mulM,
    transposeM,
    (#+#),
    (#-#),
    (#*#),
    (*#),
    (\^/),
    run,
    train,
    randomMatrix,
  )
where

import Control.Monad (replicateM)
import Control.Monad.Random (MonadRandom, randomIO)
import Data.Kind
import Data.Proxy

data Nat = Z | S Nat

type family (+) (n :: Nat) (m :: Nat) :: Nat where
  'Z + m = m
  'S n + m = 'S (n + m)

type ArrayN :: Nat -> Type -> Type
data ArrayN n a where
  Nil :: ArrayN Z a
  (:>) :: a -> ArrayN n a -> ArrayN (S n) a

infixr 5 :>

instance (Show a) => Show (ArrayN n a) where
  show Nil = "Nil"
  show (x :> xs) = show x ++ " :> " ++ show xs

instance Functor (ArrayN n) where
  fmap _ Nil = Nil
  fmap f (x :> xs) = f x :> fmap f xs

instance Foldable (ArrayN n) where
  foldr _ z Nil = z
  foldr f z (x :> xs) = f x (foldr f z xs)

instance Traversable (ArrayN n) where
  traverse _ Nil = pure Nil
  traverse f (x :> xs) = (:>) <$> f x <*> traverse f xs

head :: ArrayN (S n) a -> a
head (x :> _) = x

tail :: ArrayN (S n) a -> ArrayN n a
tail (_ :> xs) = xs

addAN :: (Num a) => ArrayN n a -> ArrayN n a -> ArrayN n a
addAN Nil Nil = Nil
addAN (x :> xs) (y :> ys) = x + y :> addAN xs ys

dot :: (Num a) => ArrayN n a -> ArrayN n a -> a
dot Nil Nil = 0
dot (x :> xs) (y :> ys) = x * y + dot xs ys

zipWithAN :: (a -> b -> c) -> ArrayN n a -> ArrayN n b -> ArrayN n c
zipWithAN _ Nil Nil = Nil
zipWithAN f (x :> xs) (y :> ys) = f x y :> zipWithAN f xs ys

type Matrix :: Nat -> Nat -> Type -> Type
data Matrix r c a where
  M :: [[a]] -> Matrix r c a

instance (Show a) => Show (Matrix r c a) where
  show (M xs) = show' xs
    where
      show' [] = ""
      show' (y : ys) = show y ++ "\n" ++ show' ys

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

type Index :: Nat -> Type
data Index n where
  IZ :: Index (S n)
  IS :: Index n -> Index (S n)

type SIndex :: Nat -> Type
data SIndex n where
  SIZ :: SIndex Z
  SIS :: SIndex n -> SIndex (S n)

fromIndex :: forall n. Index n -> Int
fromIndex IZ = 0
fromIndex (IS n) = 1 + fromIndex n

(#!!) :: Matrix r c a -> (Index r, Index c) -> a
(M mx) #!! (n, m) = (mx !! fromIndex n) !! fromIndex m

type Layer :: Nat -> Nat -> Type
data Layer inputs outputs where
  L ::
    Matrix outputs inputs Double ->
    Matrix (S Z) outputs Double ->
    Layer inputs outputs

type NeuralNetwork :: Nat -> [Nat] -> Nat -> Type
data NeuralNetwork inputs hidden outputs where
  O :: Layer inputs outputs -> NeuralNetwork inputs '[] outputs
  H ::
    Layer inputs hidden ->
    NeuralNetwork hidden hidden' outputs ->
    NeuralNetwork inputs (hidden : hidden') outputs

(#>) ::
  Layer inputs hidden ->
  NeuralNetwork hidden hidden' outputs ->
  NeuralNetwork inputs (hidden : hidden') outputs
layer #> network = H layer network

instance Functor (Matrix r c) where
  fmap f (M xs) = M $ map (map f) xs

propagate ::
  Layer inputs outputs ->
  Matrix (S Z) inputs Double ->
  Matrix (S Z) outputs Double
propagate (L weights biases) inputs = tanh <$> transposeM z
  where
    z = weights #*# transposeM inputs #+# transposeM biases

run ::
  NeuralNetwork inputs hidden outputs ->
  Matrix (S Z) inputs Double ->
  Matrix (S Z) outputs Double
run (O layer) inputs = tanh <$> propagate layer inputs
run (H layer network) inputs = run network (propagate layer inputs)

newtype NeuralNetworkConfig = NeuralNetworkConfig
  { learningRate :: Double
  }

weights :: Layer inputs outputs -> Matrix outputs inputs Double
weights (L w _) = w

biases :: Layer inputs outputs -> Matrix (S Z) outputs Double
biases (L _ b) = b

train ::
  NeuralNetworkConfig ->
  NeuralNetwork inputs hidden outputs ->
  Matrix (S Z) inputs Double ->
  Matrix (S Z) outputs Double ->
  NeuralNetwork inputs hidden outputs
train config (O layer) inputs targets = O layer'
  where
    y = propagate layer inputs
    o = tanh <$> y
    e = (o #-# targets) !! 0
    d = e #*# (1 - y ^ 2)
    deltaWeights = scaleM (learningRate config) (d #*# transposeM inputs)
    deltaBiases = scaleM (learningRate config) d
    weights' = weights layer #-# deltaWeights
    biases' = biases layer #-# deltaBiases
    layer' = L weights' biases'
train config (H layer network) inputs targets =
  H
    layer'
    (train config network (propagate layer inputs) targets)
  where
    y = propagate layer inputs
    o = tanh <$> y
    e = (o #-# targets) !! 0
    d = e #*# (1 - y ^ 2)
    deltaWeights = scaleM (learningRate config) (d #*# transposeM inputs)
    deltaBiases = scaleM (learningRate config) d
    weights' = weights layer #-# deltaWeights
    biases' = biases layer #-# deltaBiases
    layer' = L weights' biases'

class SingNat n where
  natVal :: Proxy n -> Int

instance SingNat Z where
  natVal _ = 0

instance (SingNat n) => SingNat (S n) where
  natVal _ = 1 + natVal (Proxy :: Proxy n)

randomMatrix :: (SingNat r, SingNat c, MonadRandom m) => m (Matrix r c Double)
randomMatrix = do
  let r = natVal (Proxy :: Proxy r)
  let c = natVal (Proxy :: Proxy c)
  M <$> replicateM r (replicateM c randomIO)
