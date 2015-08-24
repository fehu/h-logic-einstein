{-# LANGUAGE MultiParamTypeClasses
           , ExistentialQuantification
           , FlexibleContexts
         #-}

module Problem.Statement (

  Accessible(..)
, AccessibleDescriptor(..)
, Value(..)

, Id(..)
, IdRepr(..)

, Entry(..)
, EntryGet(..)
, EntryAccessible(..)

--, Statement(..)
--, Known(..)

) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class (Show v, Eq v) => Accessible v where
    modifiable    :: v -> Bool
    varDescriptor :: v -> AccessibleDescriptor v

newtype AccessibleDescriptor v = AccessibleDescriptor String

instance Show (AccessibleDescriptor v) where
    show (AccessibleDescriptor name) = name

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class Entry e where
    getId :: e -> Id

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class (Entry e) => EntryGet e v where
    getV     :: (Accessible v) => AccessibleDescriptor v -> e -> Maybe v
    setV     :: (Accessible v) => e -> v -> Maybe e
    clearV   :: (Accessible v) => AccessibleDescriptor v -> e -> Maybe e


class (Accessible v) => EntryAccessible e v where
    updateEntry :: v -> e -> Maybe e
    clearEntry  :: v -> e -> Maybe e

data Value e = forall v. (Show v, EntryAccessible e v) => Value v

instance Show (Value e) where show (Value v) = show v

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Id = forall i. (Show i, IdRepr i) => Id i

class IdRepr a where getRepr :: a -> String
                     getOrd  :: a -> Int

isSameId :: (IdRepr a, IdRepr b) => a -> b -> Bool
isSameId x y = getRepr x == getRepr y

instance Show Id where show (Id i)        = show i
instance Eq   Id where (Id i1) == (Id i2) = isSameId  i1 i2
instance Ord  Id where (Id i1) <= (Id i2) = getOrd i1 <= getOrd i2


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

--data Statement v a = Atomic (v, a)
--                   | Constraint v (a -> Bool)
--                   | And [Statement v a]
--                   | Or [Statement v a]
--
--data Known v a = Known (Statement v a) (Statement v a)
--               | Condition (Known v a) v (a -> a -> Bool)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --




