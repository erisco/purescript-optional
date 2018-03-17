module Data.Optional
  ( Optional(Optional)
  , default
  , use
  , optional'
  , optional
  , fromMaybe
  , toMaybe
  )
where

import Control.Alt (class Alt, map)
import Control.Alternative (class Alternative, pure)
import Control.Apply (lift2)
import Control.Extend (class Extend)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Enum (class Enum, pred, succ)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Generic as Fgn
import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Data.Functor.Invariant (class Invariant)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Sum(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (class Monoid, append, mempty, (<>))
import Data.Ord (class Ord1, Ordering(..), compare)
import Data.Traversable (class Traversable)
import Prelude (class Applicative, class Apply, class Bind, class Bounded, class Eq, class Functor, class Monad, class Ord, class Semigroup, class Show, bottom, eq, show, top, (==))

-- | `Optional` is a newtype on the uncurried Church encoding of `Maybe`.
newtype Optional a = Optional (forall r. Fn2 r (a -> r) r)

-- | `Optional a` default constructor. Analogous to `Nothing`.
default :: forall a. Optional a
default = Optional (mkFn2 (\z _ -> z))

-- | `Optional a` use constructor. Analogous to `Just`.
use :: forall a. a -> Optional a
use x = Optional (mkFn2 (\_ f -> f x))

-- | `Optional a` destructor.
optional' :: forall a r. Optional a -> Fn2 r (a -> r) r
optional' (Optional x) = x

-- | A variant `Optional a` destructor analogous to `maybe`.
optional :: forall a r. r -> (a -> r) -> Optional a -> r
optional z f (Optional x) = runFn2 x z f

instance functorOptional :: Functor Optional where
  map f (Optional o) = runFn2 o default (\x -> use (f x))
--

instance applyOptional :: Apply Optional where
  apply (Optional o) (Optional p) =
    runFn2 o default (\f -> runFn2 p default (\x -> use (f x)))
--

instance applicativeOptional :: Applicative Optional where
  pure = use
--

instance altOptional :: Alt Optional where
  alt (Optional o) p = runFn2 o p use
--

instance plusOptional :: Plus Optional where
  empty = default
--

instance alternativeOptional :: Alternative Optional

instance bindOptional :: Bind Optional where
  bind (Optional o) f = runFn2 o default f
--

instance monadOptional :: Monad Optional

instance monadZeroOptional :: MonadZero Optional

instance extendOptional :: Extend Optional where
  extend f o = use (f o)
--

instance invariantOptional :: Invariant Optional where
  imap f _ o = map f o
--

instance semigroupOptional :: Semigroup a => Semigroup (Optional a) where
  append = lift2 append
--

instance monoidOptional :: Semigroup a => Monoid (Optional a) where
  mempty = default
--

instance eqOptional :: Eq a => Eq (Optional a) where
  eq (Optional o) (Optional p) =
    runFn2 o
    (runFn2 p true (\_ -> false))
    (\x -> runFn2 p false (\y -> x == y))
--

instance eq1Optional :: Eq1 Optional where
  eq1 = eq
--

instance ordOptional :: Ord a => Ord (Optional a) where
  compare (Optional o) (Optional p) =
    runFn2 o
    (runFn2 p EQ (\_ -> LT))
    (\x -> runFn2 p GT (\y -> compare x y))
--

instance ord1Optional :: Ord1 Optional where
  compare1 = compare
--

instance boundedOptional :: Bounded a => Bounded (Optional a) where
  top = use top
  bottom = default
--

instance enumOptional :: (Bounded a, Enum a) => Enum (Optional a) where
  succ (Optional o) = runFn2 o (Just (use bottom)) (\x -> map use (succ x))
  pred (Optional o) =
    runFn2 o
    Nothing
    (\x' -> maybe (Just default) (\x -> Just (use x)) (pred x'))
--

instance showOptional :: Show a => Show (Optional a) where
  show (Optional o) = runFn2 o "default" (\x -> "(use " <> show x <> ")")
--

instance genericOptional ::
  Generic (Optional a)
  (Sum (Constructor "Default" NoArguments) (Constructor "Use" (Argument a)))
  where
  to (Inl _) = default
  to (Inr (Constructor (Argument x))) = use x
  from (Optional o) =
    runFn2 o
    (Inl (Constructor NoArguments))
    (\x -> Inr (Constructor (Argument x)))
--

instance encodeOptional :: Encode a => Encode (Optional a) where
  encode = Fgn.genericEncode Fgn.defaultOptions
--

instance decodeOptional :: Decode a => Decode (Optional a) where
  decode = Fgn.genericDecode Fgn.defaultOptions
--

instance foldableOptional :: Foldable Optional where
  foldMap f (Optional o) = runFn2 o mempty f
  foldr f z (Optional o) = runFn2 o z (\x -> f x z)
  foldl f z (Optional o) = runFn2 o z (f z)
--

instance traversableOptional :: Traversable Optional where
  traverse f (Optional o) = runFn2 o (pure default) (\x -> map use (f x))
  sequence (Optional o) = runFn2 o (pure default) (map use)
--

-- | Convert `Maybe a` to `Optional a`.
fromMaybe :: forall a. Maybe a -> Optional a
fromMaybe = maybe default use

-- | Convert `Optional a` to `Maybe a`.
toMaybe :: forall a. Optional a -> Maybe a
toMaybe = optional Nothing Just
