{-# LANGUAGE MultiParamTypeClasses
           , ExistentialQuantification
         #-}

module Problem.Statement (

  Accessible(..)
, AccessibleDescriptor(..)
, Value(..)

, Id(..)
, Entry(..)
, EntryGet(..)
, EntryId(..)

, Statement(..)
, Known(..)

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



data Id = forall i. (Show i, Eq i, Ord i) => Id i

class (Entry e) => EntryId e where
    getId :: e -> Id

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Statement v a = Atomic (v, a)
                   | Constraint v (a -> Bool)
                   | And [Statement v a]
                   | Or [Statement v a]

data Known v a = Known (Statement v a) (Statement v a)
               | Condition (Known v a) v (a -> a -> Bool)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

