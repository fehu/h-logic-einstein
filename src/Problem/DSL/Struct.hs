{-# LANGUAGE MultiParamTypeClasses
           , ExistentialQuantification
         #-}

module Problem.DSL.Struct (

  DSLStatement(..)
, DSLKnown(..)
, DSLCondition1(..)
, DSLCondition2(..)
, DSLKnownCond1(..)
, DSLKnownCond2(..)

, DSLContainer(..)
, DSLExpression(..)
, DSLContainerContainer(..)

) where

import Problem.Statement
import Problem.Exec


data DSLStatement v = DSLAtomic v
                    | DSLConstraint (v -> Bool)
                    | DSLAnd [DSLStatement v]
                    | DSLOr  [DSLStatement v]

data DSLKnown a b = DSLKnown (DSLStatement a) (DSLStatement b)

data DSLCondition1 v     = DSLCondition1 (Maybe v -> Maybe v -> Bool)
data DSLCondition2 v1 v2 = DSLCondition2 ((v1,v2) -> (v1,v2) -> Bool)

data DSLKnownCond1 a b v     = DSLKnownCond1 (DSLKnown a b) (DSLCondition1 v)
data DSLKnownCond2 a b v1 v2 = DSLKnownCond2 (DSLKnown a b) (DSLCondition2 v1 v2)


class DSLContainer c e where
    test :: e -> e -> c e -> SApplyResult Value

data DSLContainerContainer e = forall c. (DSLContainer c e, Show (c e)) => DSLC (c e)

instance Show (DSLContainerContainer e) where
    show (DSLC c) = show c



class DSLExpression expr e where
    boxExpression :: expr -> DSLContainerContainer e


