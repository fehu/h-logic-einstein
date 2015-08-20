{-# LANGUAGE MultiParamTypeClasses
           , ExistentialQuantification
           , FlexibleInstances
--           , FlexibleContexts
--           , UndecidableInstances
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
import ProblemStatement.Exec


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
    test :: e -> e -> c e -> SApplyResult Value



instance (EntryId e) => DSLContainer DSLKnownContainer e where
    test e1 e2 (DSLKnownContainer a fa b fb) =
        case (left, right) of (Just True,  Nothing  )  -> SImplies  [p2] [p1]
                              (Nothing,    Just True)  -> SImplies  [p1] [p2]
                              (Just True,  Just True)  -> SConfirm  [p1, p2]
                              (Just False, Just False) -> SEmpty    [p1, p2]
                              (Nothing,    Nothing)    -> SPossible [p1, p2]
                              _                        -> SBroken   [p1, p2]
        where left  = fmap (== a) (fa e1)
              right = fmap (== b) (fb e2)
              p1 = (getId e1, [Value a])
              p2 = (getId e2, [Value b])

instance DSLContainer DSLCondContainer1 e where

instance DSLContainer DSLKnownCondContainer1 e where

data DSLContainerContainer e = forall c. (DSLContainer c e, Show (c e)) => DSLC (c e)

data DSLKnownContainer entry = forall a b. ( Eq a, Accessible a, Show a
                                           , Eq b, Accessible b, Show b) =>
                                 DSLKnownContainer a (entry -> Maybe a)
                                                   b (entry -> Maybe b)

data DSLCondContainer1 entry = forall v. DSLCondContainer1 (Maybe v -> Maybe v -> Bool)
                                                           (entry -> Maybe v)
                                                           (AccessibleDescriptor v)

data DSLKnownCondContainer1 entry = DSLKnownCondContainer1 (DSLKnownContainer entry)
                                                           (DSLCondContainer1 entry)

instance Show (DSLContainerContainer e) where
    show (DSLC c) = show c
instance Show (DSLKnownContainer e) where
    show (DSLKnownContainer a _ b _) = show a ++ " <==> " ++ show b
instance Show (DSLCondContainer1 e) where
    show (DSLCondContainer1 _ _ (AccessibleDescriptor v)) = "Condition over v"
instance Show (DSLKnownCondContainer1 e) where
    show (DSLKnownCondContainer1 known cond) = "[" ++ show known ++ ", " ++ show cond ++ "]"

testCCond1 :: DSLCondContainer1 e -> e -> e -> Bool
testCCond1 (DSLCondContainer1 f get _) e1 e2 = f (get e1) (get e2)





class DSLExpression expr e where
    boxExpression :: expr -> DSLContainerContainer e

instance ( Accessible a, EntryGet e a
         , Accessible b, EntryGet e b
         , EntryId e) =>
    DSLExpression (DSLKnown a b) e
    where
        boxExpression = DSLC . dsl2CKnown


instance (Accessible v, EntryGet e v) => DSLExpression (DSLCondition1 v) e where
    boxExpression = DSLC . dsl2CCond1

instance ( Accessible a, EntryGet e a
         , Accessible b, EntryGet e b
         , Accessible v, EntryGet e v) =>
    DSLExpression (DSLKnownCond1 a b v) e
    where
        boxExpression (DSLKnownCond1 known cond) = DSLC $ DSLKnownCondContainer1 k c
                                                where k = dsl2CKnown known
                                                      c = dsl2CCond1 cond
--(DSLKnown a b) (DSLCondition2 v1 v2)

cStatement :: (EntryGet entry v, Accessible v) => DSLStatement v -> (v, entry -> Maybe v)
cStatement (DSLAtomic v) = (v, getV $ varDescriptor v)
-- TODO

dsl2CKnown (DSLKnown s1 s2) = DSLKnownContainer a ga b gb
                                      where (a, ga) = cStatement s1
                                            (b, gb) = cStatement s2

cCond1 vd f = DSLCondContainer1 f (getV vd) vd

dsl2CCond1 :: (EntryGet entry v, Accessible v) => DSLCondition1 v -> DSLCondContainer1 entry
dsl2CCond1 (DSLCondition1 f) = cCond1 (varDescriptor (undefined::v)) f




data Rule e = Rule { ruleName        :: String
                   , ruleDescription :: Maybe String
                   , ruleDef         :: DSLContainerContainer e
                   }
                   deriving Show

type KnownFacts e = [Rule e]

name -: expr = Rule name Nothing (boxExpression expr)
rule |:: descr = rule { ruleDescription = Just descr }

infix 3 -:
infix 2 |::

--dslToInternal facts =




