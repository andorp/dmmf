module Data.Bifunctor

interface Bifunctor (p : Type -> Type -> Type) where
  bimap : (a -> b) -> (c -> d) -> p a c -> p b d

  first : (a -> b) -> p a c -> p b c
  first f = bimap f id

  second : (c -> d) -> p a c -> p a d
  second g = bimap id g

