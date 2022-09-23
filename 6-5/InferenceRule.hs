{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyCase #-}

module InferenceRule
  ( People(..)
  , Food(..)
  , Eat(..)
  , NotEat(..)
  ) where

data (:==:) :: k -> k -> * where
  Refl :: a :==: a

data Bottom

type Not p = p -> Bottom

type a :/=: b = Not (a :==: b)

class (a :: k) `Neq` (b :: k) where
  neq :: a :/=: b
  neq x = case x of {}

data People = Tonkichi
            | Chinpei
            | Kenta
              deriving (Eq, Show)

instance Tonkichi `Neq` Chinpei
instance Tonkichi `Neq` Kenta
instance Chinpei `Neq` Kenta
instance Chinpei `Neq` Tonkichi
instance Kenta `Neq` Tonkichi
instance Kenta `Neq` Chinpei

data Food = Curry
          | Soba
          | Ramen
            deriving (Eq, Show)

instance Curry `Neq` Soba
instance Curry `Neq` Ramen
instance Soba `Neq` Ramen
instance Soba `Neq` Curry
instance Ramen `Neq` Curry
instance Ramen `Neq` Soba

data Eat :: People -> Food -> * where
  EatRemainFood :: ( f1 `Neq` f2
                   , f2 `Neq` f3
                   , f3 `Neq` f1
                   ) =>
                   p `NotEat` f1
                -> p `NotEat` f2
                -> p `Eat` f3

  RemainPeopleEat :: ( p1 `Neq` p2
                   , p2 `Neq` p3
                   , p3 `Neq` p1
                   ) =>
                   p1 `NotEat` f
                -> p2 `NotEat` f
                -> p3 `Eat` f
  
data NotEat :: People -> Food -> * where
  NotEatAnotherFood :: ( f1 `Neq` f2
                       ) =>
                       p `Eat` f1
                    -> p `NotEat` f2
  AnotherPeopleNotEat :: ( p1 `Neq` p2
                       ) =>
                       p1 `Eat` f
                    -> p2 `NotEat` f
