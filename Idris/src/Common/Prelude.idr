module Common.Prelude -- 'where'

import Data.Either
import Data.Vect
import Data.List

public export
Flip : (Type -> Type -> Type) -> Type -> Type -> Type
Flip f x y = f y x

public export
data Result : Type -> Type -> Type where
  Ok    : success -> Result success failure
  Error : failure -> Result success failure

public export
(Show success, Show failure) =>
Show (Result success failure) where
  show (Ok s)    = "Ok " ++ show s
  show (Error e) = "Error " ++ show e

public export
[ResultSuccessFunctor] Functor (Flip Result failure) where
  map f (Ok x)    = Ok (f x)
  map f (Error e) = Error e

public export
[ResultFailureFunctor] Functor (Result success) where
  map f (Ok x)    = Ok x
  map f (Error e) = Error (f e)

public export
[ResultFailureFoldable] Foldable (Result success) where
  foldr f init (Ok _)    = init
  foldr f init (Error e) = f e init
  foldl f init (Ok _)    = init
  foldl f init (Error e) = f init e

public export
[ResultSuccessFoldable] Foldable (Flip Result failure) where
  foldr f init (Ok x)    = f x init
  foldr f init (Error _) = init
  foldl f init (Ok x)    = f init x
  foldl f init (Error _) = init

public export
[ResultFailureTraversable]
(Functor (Result success), Foldable (Result success)) =>
Traversable (Result success) where
  traverse f (Ok x)    = pure (Ok x)
  traverse f (Error e) = Error <$> f e

public export
[ResultSuccessTraversable]
(Functor (Flip Result failure), Foldable (Flip Result failure)) =>
Traversable (Flip Result failure) where
  traverse f (Ok x)    = Ok <$> f x
  traverse f (Error e) = pure (Error e)

export
partitionResults : List (Result o e) -> (List o, List e)
partitionResults = partitionEithers . map toEither
  where
    toEither : Result o e -> Either o e
    toEither (Ok o)    = Left o
    toEither (Error e) = Right e

public export
record DateTime where
  constructor MkDateTime
  Epoctime : Int

public export
record Command d where
  constructor MkCommand
  Data      : d
  Timestamp : DateTime
  UserId    : String
  -- ETC...

namespace String50
  public export
  data String50Error : Type where
    MkString50Error : String -> String50Error

  export
  create : String -> Result String String50Error
  create s = if length s > 50
    then Error $ MkString50Error "Longer than 50"
    else Ok s

  export
  createMaybe : String -> Result (Maybe String) String50Error
  createMaybe s =
    if s == "" then Ok Nothing
    else if length s > 50 then Ok $ Just s
    else Error $ MkString50Error "Longer than 50"

