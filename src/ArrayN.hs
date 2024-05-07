{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module ArrayN where

import Data.Kind
import Data.Proxy
import GHC.TypeNats

data CNat = Z | S CNat

type CountN :: CNat -> Nat
type family CountN n where
  CountN Z = 0
  CountN (S n) = 1 + CountN n

type MountN :: Nat -> CNat
type family MountN n where
  MountN 0 = Z
  MountN n = S (MountN (n - 1))

type ArrayN :: CNat -> Type -> Type
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

length :: forall a n. (KnownNat n) => ArrayN (MountN n) a -> Int
length _ = fromIntegral $ natVal (Proxy @n)

addAN :: (Num a) => ArrayN n a -> ArrayN n a -> ArrayN n a
addAN Nil Nil = Nil
addAN (x :> xs) (y :> ys) = x + y :> addAN xs ys

dot :: (Num a) => ArrayN n a -> ArrayN n a -> a
dot Nil Nil = 0
dot (x :> xs) (y :> ys) = x * y + dot xs ys

zipWithAN :: (a -> b -> c) -> ArrayN n a -> ArrayN n b -> ArrayN n c
zipWithAN _ Nil Nil = Nil
zipWithAN f (x :> xs) (y :> ys) = f x y :> zipWithAN f xs ys
