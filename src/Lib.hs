{-# LANGUAGE UndecidableInstances, InstanceSigs, DataKinds, TypeFamilies, KindSignatures, GADTs #-}
module Lib where
import Data.Semigroup ((<>))
data Unit = MkUnit
data Boll = True | False

data Nat = Zero | Succ Nat

data Vector n a where
    VNil :: Vector Zero a
    VCons :: a -> Vector n a -> Vector (Succ n) a

instance Show a => Show (Vector n a) where
    show :: Vector n a -> String
    show VNil = "Vnil"
    show (VCons a as) = "VCons" <> (show a)  <> " " <> show as

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ n) m = add n (Succ m)

type family Adder n m where
    Adder 'Zero m = m
    Adder ('Succ n) m = Adder n ('Succ m)

type family Addest n m where
    Addest 'Zero m = m
    Addest ('Succ n) m = 'Succ (Addest n m)

append :: Vector n a -> Vector m a -> Vector (Addest n m) a
append VNil xs = xs
append (VCons x rest) xs= VCons x (append rest xs)

someFunc :: IO ()
someFunc = undefined
