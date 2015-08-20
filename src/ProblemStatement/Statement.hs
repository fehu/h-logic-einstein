{-# LANGUAGE MultiParamTypeClasses
           , ExistentialQuantification
         #-}

module ProblemStatement.Statement (

  Accessible(..)
, Value(..)
, AccessibleDescriptor(..)

--, Value(..)

, Id(..)
, Entry(..)
, EntryGet(..)
, EntryId(..)

, Statement(..)
, Known(..)

) where


class (Show v, Eq v) => Accessible v where
    modifiable    :: v -> Bool
    varDescriptor :: v -> AccessibleDescriptor v

--newtype (Show v, Eq v) => Var v = Var v deriving (Show, Eq, Ord)
--newtype (Show i, Eq i) => Id  i = Id  i deriving (Show, Eq, Ord)

--instance (Show v, Eq v) => Accessible (Var v) where
--    modifiable _   = True
--    varDescripor _ = AccessibleDescriptor
--instance (Show i, Eq i) => Accessible (Id  i) where modifiable _ = False


data Value = forall v. (Accessible v) => Value v

newtype AccessibleDescriptor v = AccessibleDescriptor String

class Entry e where
    get   :: (Accessible v, EntryGet e v) => AccessibleDescriptor v -> e -> e

class (Entry e) => EntryGet e v where
    getV :: (Accessible v) => AccessibleDescriptor v -> e -> Maybe v -- AccessibleDescriptor v ->



data Id = forall i. (Show i, Eq i, Ord i) => Id i
--        deriving (Show, Eq, Ord)

class (Entry e) => EntryId e where -- , Accessible id, EntryGet e id
    getId :: e -> Id

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Statement v a = Atomic (v, a)
                   | Constraint v (a -> Bool)
                   | And [Statement v a]
                   | Or [Statement v a]


--newtype Known v a = Known (Statement v a, Statement v a)
data Known v a = Known (Statement v a) (Statement v a)
               | Condition (Known v a) v (a -> a -> Bool)


