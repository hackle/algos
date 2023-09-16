{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds, RankNTypes, PolyKinds, StandaloneKindSignatures, GADTs #-}

module Algo.Printf where

-- type Append :: forall a. [a] -> [a] -> [a]
type family Append xs ys where
    Append '[] ys = ys
    Append (x:xs) ys = x : Append xs ys

-- type Alt :: forall a. Maybe a -> Type
type family Alt a where
    Alt (Maybe a) = [a]
    Alt [a] = Maybe a
    
-- type HList :: [Type] -> Type
data HList xs where
    HNil :: HList '[]
    (:&) :: x -> HList xs -> HList (x:xs)
infixr 5 :&

h1 :: HList [Int, String]
h1 = 1 :& "foo" :& HNil

foo :: Maybe a -> Alt (Maybe a)
foo Nothing = []
foo (Just a) = [a]

bar :: [a] -> Alt [a]
bar [] = Nothing
bar (x:_) = Just x

fooBar :: a -> Alt [a]
fooBar x = Just x

type family PF a where
    PF "%s" = String -> String
    PF xs = String

printf :: a -> PF a
printf x = undefined

fooStr :: "Foo"
fooStr = "Foo"