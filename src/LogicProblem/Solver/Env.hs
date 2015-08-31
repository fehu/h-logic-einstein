{-# LANGUAGE MultiParamTypeClasses
           , ExistentialQuantification
         #-}

module LogicProblem.Solver.Env (

  Accessible(..)
, AccessibleDescriptor(..)
, Value(..)

, Id(..)
, IdRepr(..)

, Entry(..)
, AccessibleEntry(..)

, ETable(..)
, newETable
, getEntry
, setEntry

) where

import qualified Data.Map as M

import Data.List (intercalate)

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

class (Entry e, Accessible v) => AccessibleEntry e v where
    getV     :: AccessibleDescriptor v -> e -> Maybe v
    setV     :: e -> v -> Maybe e
    clearV   :: AccessibleDescriptor v -> e -> Maybe e

data Value e = forall v. (Show v, AccessibleEntry e v) => Value v

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

newtype ETable e = ETable (M.Map Id e)

newETable :: (a -> (Id, e)) -> [a] -> ETable e
newETable f as = ETable $ M.fromList (map f as)

getEntry :: Id -> ETable e -> e
getEntry id (ETable mp) = mp M.! id

setEntry :: (Entry e) => e -> ETable e -> ETable e
setEntry e (ETable mp) = ETable $ M.adjust (const e) (getId e) mp

instance (Show e) => Show (ETable e) where
    show (ETable mp) = intercalate "\n" $ map show (M.elems mp)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --




