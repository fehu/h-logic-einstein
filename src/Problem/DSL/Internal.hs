{-# LANGUAGE MultiParamTypeClasses
           , ExistentialQuantification
           , FlexibleInstances
         #-}


module Problem.DSL.Internal (

) where

import Problem.DSL.Struct
import Problem.Statement
import Problem.Exec


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Containers

data DSLKnownContainer entry = forall a b. ( Eq a, Accessible a, Show a
                                           , Eq b, Accessible b, Show b) =>
                                 DSLKnownContainer a (entry -> Maybe a)
                                                   b (entry -> Maybe b)

instance Show (DSLKnownContainer e) where
    show (DSLKnownContainer a _ b _) = show a ++ " <==> " ++ show b


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

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data DSLCondContainer1 entry = forall v. DSLCondContainer1 (Maybe v -> Maybe v -> Bool)
                                                           (entry -> Maybe v)
                                                           (AccessibleDescriptor v)

instance Show (DSLCondContainer1 e) where
    show (DSLCondContainer1 _ _ (AccessibleDescriptor v)) = "Condition over v"

instance (EntryId e) => DSLContainer DSLCondContainer1 e where
    test e1 e2 c | testCCond1 c e1 e2 = SConfirm p
                 | otherwise          = SBroken  p
        where p = map (\e -> (getId e, [])) [e1, e2]

testCCond1 :: DSLCondContainer1 e -> e -> e -> Bool
testCCond1 (DSLCondContainer1 f get _) e1 e2 = f (get e1) (get e2)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data DSLKnownCondContainer1 entry = DSLKnownCondContainer1 (DSLKnownContainer entry)
                                                           (DSLCondContainer1 entry)

instance Show (DSLKnownCondContainer1 e) where
    show (DSLKnownCondContainer1 known cond) = "[" ++ show known ++ ", " ++ show cond ++ "]"

instance (EntryId e) => DSLContainer DSLKnownCondContainer1 e where
    test e1 e2 (DSLKnownCondContainer1 kc cc) =
        if isSuccess known then if      isSuccess      cond then known
                                else if isUndetermined cond then poss
                                                            else cond
                           else known
        where known = test e1 e2 kc
              cond  = test e1 e2 cc
              poss  = SPossible $ getResultEntries known

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- instance DSLExpression

instance ( Accessible a, EntryGet e a
         , Accessible b, EntryGet e b
         , EntryId e) =>
    DSLExpression (DSLKnown a b) e
    where
        boxExpression = DSLC . dsl2CKnown


instance (Accessible v, EntryGet e v, EntryId e) => DSLExpression (DSLCondition1 v) e where
    boxExpression = DSLC . dsl2CCond1

instance ( Accessible a, EntryGet e a
         , Accessible b, EntryGet e b
         , Accessible v, EntryGet e v
         , EntryId e ) =>
    DSLExpression (DSLKnownCond1 a b v) e
    where
        boxExpression (DSLKnownCond1 known cond) = DSLC $ DSLKnownCondContainer1 k c
                                                where k = dsl2CKnown known
                                                      c = dsl2CCond1 cond

cStatement :: (EntryGet entry v, Accessible v) => DSLStatement v -> (v, entry -> Maybe v)
cStatement (DSLAtomic v) = (v, getV $ varDescriptor v)
-- TODO

dsl2CKnown (DSLKnown s1 s2) = DSLKnownContainer a ga b gb
                                      where (a, ga) = cStatement s1
                                            (b, gb) = cStatement s2

cCond1 vd f = DSLCondContainer1 f (getV vd) vd

dsl2CCond1 :: (EntryGet entry v, Accessible v) => DSLCondition1 v -> DSLCondContainer1 entry
dsl2CCond1 (DSLCondition1 f) = cCond1 (varDescriptor (undefined::v)) f

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --



