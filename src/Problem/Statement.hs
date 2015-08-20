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
, EntryId(..)

--, Statement(..)
--, Known(..)

) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class (Show v, Eq v) => Accessible v where
    modifiable    :: v -> Bool
    varDescriptor :: v -> AccessibleDescriptor v

data Value = forall v. (Accessible v) => Value v

newtype AccessibleDescriptor v = AccessibleDescriptor String

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class Entry e where
    get   :: (Accessible v, EntryGet e v) => AccessibleDescriptor v -> e -> e

class (Entry e) => EntryGet e v where
    getV :: (Accessible v) => AccessibleDescriptor v -> e -> Maybe v



class IdRepr a where getRepr :: a -> String
                     getOrd  :: a -> Int

isSameId :: (IdRepr a, IdRepr b) => a -> b -> Bool
isSameId x y = getRepr x == getRepr y

data Id = forall i. (Show i, IdRepr i) => Id i

instance Show Id where show (Id i)        = show i
instance Eq   Id where (Id i1) == (Id i2) = isSameId  i1 i2
instance Ord  Id where (Id i1) <= (Id i2) = getOrd i1 <= getOrd i2

class (Entry e) => EntryId e where
    getId :: e -> Id

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

--data Statement v a = Atomic (v, a)
--                   | Constraint v (a -> Bool)
--                   | And [Statement v a]
--                   | Or [Statement v a]
--
--data Known v a = Known (Statement v a) (Statement v a)
--               | Condition (Known v a) v (a -> a -> Bool)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

