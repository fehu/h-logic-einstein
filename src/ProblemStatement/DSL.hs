{-# LANGUAGE MultiParamTypeClasses
           , ExistentialQuantification
           , FlexibleInstances
--           , ScopedTypeVariables
         #-}

module ProblemStatement.DSL (

--  DSLStatement(..)
--, DSLKnown(..)

 KnownFacts

, (<==>)
, (|?>)

, (-:)
, (|::)

) where

import ProblemStatement.Statement

data DSLStatement v = DSLAtomic v
                    | DSLConstraint (v -> Bool)
                    | DSLAnd [DSLStatement v]
                    | DSLOr  [DSLStatement v]

data DSLKnown a b = DSLKnown (DSLStatement a) (DSLStatement b)

data DSLCondition1 v     = DSLCondition1 (Maybe v -> Maybe v -> Bool)
data DSLCondition2 v1 v2 = DSLCondition2 ((v1,v2) -> (v1,v2) -> Bool)

data DSLKnownCond1 a b v     = DSLKnownCond1 (DSLKnown a b) (DSLCondition1 v)
data DSLKnownCond2 a b v1 v2 = DSLKnownCond2 (DSLKnown a b) (DSLCondition2 v1 v2)




x <==> y = DSLKnown (DSLAtomic x) (DSLAtomic y)
k |?> f  = DSLKnownCond1 k (DSLCondition1 f)

class DSLContainer c e where
    test :: e -> e -> c e -> Bool

instance DSLContainer DSLKnownContainer e


data DSLContainerContainer e = forall c. (DSLContainer c e) => DSLKC (c e)

data DSLKnownContainer entry = forall a b. DSLKnownContainer a (entry -> Maybe a)
                                                             b (entry -> Maybe b)

data ConditionContainer1 entry = forall v. ConditionContainer1 (Maybe v -> Maybe v -> Bool)
                                                               (entry -> Maybe v)

testCCond1 :: ConditionContainer1 e -> e -> e -> Bool
testCCond1 (ConditionContainer1 f get) e1 e2 = f (get e1) (get e2)





class DSLExpression expr e where
    boxExpression :: expr -> DSLContainerContainer e

instance (Accessible a, Accessible b, EntryGet e a, EntryGet e b) => DSLExpression (DSLKnown a b) e where
    boxExpression (DSLKnown s1 s2) = DSLKC $ DSLKnownContainer a ga b gb
                                  where (a, ga) = cStatement s1
                                        (b, gb) = cStatement s2


instance DSLExpression (DSLCondition1 v) e where

instance DSLExpression (DSLKnownCond1 a b v) e where


cStatement :: (EntryGet entry v, Accessible v) => DSLStatement v -> (v, entry -> Maybe v)
cStatement (DSLAtomic v) = (v, getV $ varDescriptor v)

--cKnown vd1 vd2 v1 v2 = DSLKnownContainer (get vd1) v1 (get vd2) v2
cCond1 vd f = ConditionContainer1 f (getV vd)

dsl2CCond1 :: (EntryGet entry v, Accessible v) => DSLCondition1 v -> ConditionContainer1 entry
dsl2CCond1 (DSLCondition1 f) = cCond1 (varDescriptor (undefined::v)) f




data Rule e = Rule { ruleName        :: String
                   , ruleDescription :: Maybe String
                   , ruleDef         :: DSLContainerContainer e
                   }

type KnownFacts e = [Rule e]

name -: expr = Rule name Nothing (boxExpression expr)
rule |:: descr = rule { ruleDescription = Just descr }

infix 3 -:
infix 2 |::

--dslToInternal facts =




