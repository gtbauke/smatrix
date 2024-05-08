{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Matrix where

import Data.Kind
import GHC.TypeLits

type Matrix :: Nat -> Nat -> Type -> Type
data Matrix r c a where
  M :: [[a]] -> Matrix r c a

deriving instance (Show a) => Show (Matrix r c a)

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
  IZ :: Index (n + 1)
  IS :: Index n -> Index (n + 1)

fromIndex :: forall n. Index n -> Int
fromIndex IZ = 0
fromIndex (IS n) = 1 + fromIndex n

(#!!) :: Matrix r c a -> (Index r, Index c) -> a
(M mx) #!! (n, m) = (mx !! fromIndex n) !! fromIndex m

-- TODO: maybe it is possible to make a index function that takes two integers and
-- TODO: is type safe still (using Singleton types)

-- TODO: matrix typed by the user
-- TODO: linear system of equations represented by matrix
-- TODO: networks represented by matrices
