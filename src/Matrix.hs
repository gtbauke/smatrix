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
