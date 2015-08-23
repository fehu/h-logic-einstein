{-# LANGUAGE MultiParamTypeClasses
           , ExistentialQuantification
           , FlexibleInstances
         #-}


module Problem.DSL.Internal (

) where

import Data.Maybe (maybeToList)

import Problem.DSL.Struct
import Problem.Statement
import Problem.Exec


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Containers



data DSLKnownContainer entry = forall a b. ( Eq a, EntryAccessible entry a, Show a
                                           , Eq b, EntryAccessible entry b, Show b) =>
                                 DSLKnownAtomsContainer a (entry -> Maybe a)
                                                        b (entry -> Maybe b)
                             | forall a b. ( Eq a, EntryAccessible entry a, Show a
                                           , Eq b, EntryAccessible entry b) =>
                                  DSLKnownConstraintContainer  a                 (entry -> Maybe a)
                                                               (Maybe b -> Bool) (entry -> Maybe b)

instance Show (DSLKnownContainer e) where
    show (DSLKnownAtomsContainer a _ b _) = show a ++ " <==> " ++ show b


applyKC' eL eR (a, ga) (b, gb) =
    case (left, right) of (Just True,  Nothing  )  -> SImplies  [implR] pr
                          (Nothing,    Just True)  -> SImplies  [implL] pr
                          (Just True,  Just True)  -> SConfirm  pr
                          (Just False, Just False) -> SBroken   pr
                          (Nothing,    Nothing)    -> SEmpty    pr
                          _                        -> SBroken   pr
    where vA = ga eL
          vB = gb eR
          left  = fmap (== a) vA
          right = fmap (== b) vB
          implL = (getId eL, [Value a])
          implR = (getId eR, [Value b])
          pL    = (getId eL, mbValList vA)
          pR    = (getId eR, mbValList vB)
          pr    = [pL, pR]


mbValList v = maybeToList $ fmap Value v

applyKC1 :: (Entry e) => e -> DSLKnownContainer e -> SApplyResult (Value e)
applyKC1 e (DSLKnownAtomsContainer a ga b gb) = applyKC' e e (a, ga) (b, gb)
applyKC1 e (DSLKnownConstraintContainer a ga f gv) =
    if f . gv $ e then case fmap (== a) (ga e) of Just True  -> SConfirm pr
                                                  Just False -> SBroken  pr
                                                  _          -> SImplies [pi] pr
                  else SEmpty [p, pC]
    where p  = (getId e, mbValList $ ga e)
          pC = (getId e, mbValList $ gv e)
          pi = (getId e, [Value a])
          pr = [p, pC]

applyKC2 :: (Entry e) => e -> e -> DSLKnownContainer e -> SApplyResult (Value e)
applyKC2 e1 e2 (DSLKnownAtomsContainer a ga b gb) = applyKC' e1 e2 (a, ga) (b, gb)

instance (Entry e) => DSLContainer DSLKnownContainer e where
    applyC kc = SApply1 (`applyKC1` kc)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

--data DSLConstraintContainer entry = forall a b. ( Eq a, EntryAccessible entry a, Show a
--                                                , Eq b, EntryAccessible entry b, Show b) =>
--                                 DSLConstraintContainer a                 (entry -> Maybe a)
--                                                        (Maybe b -> Bool) (entry -> Maybe b)


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data DSLCondContainer1 entry = forall v. DSLCondContainer1 (Maybe v -> Maybe v -> Bool)
                                                           (entry -> Maybe v)
                                                           (AccessibleDescriptor v)

instance Show (DSLCondContainer1 e) where
    show (DSLCondContainer1 _ _ (AccessibleDescriptor v)) = "Condition over " ++ show v

instance (Entry e) => DSLContainer DSLCondContainer1 e where
    applyC c = SApply2 (applyCC1 c)

applyCC1 c e1 e2 | testCCond1 c e1 e2 = SConfirm $ p e1 e2
                 | otherwise          = SEmpty   $ p e1 e2
    where p e1 e2 = map (\e -> (getId e, [])) [e1, e2]

testCCond1 :: DSLCondContainer1 e -> e -> e -> Bool
testCCond1 (DSLCondContainer1 f get _) e1 e2 = f (get e1) (get e2)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data DSLKnownCondContainer1 entry = DSLKnownCondContainer1 (DSLKnownContainer entry)
                                                           (DSLCondContainer1 entry)

instance Show (DSLKnownCondContainer1 e) where
    show (DSLKnownCondContainer1 known cond) = "[" ++ show known ++ ", " ++ show cond ++ "]"

instance (Entry e) => DSLContainer DSLKnownCondContainer1 e where
    applyC (DSLKnownCondContainer1 kc cc) = SApply2 apply
        where apply e1 e2 | isSuccess known && isSuccess      cond = known
                          | isSuccess known                        = cond
                          | otherwise                              = known
                  where known = applyKC2 e1 e2 kc
                        cond  = applyCC1 cc e1 e2

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- instance DSLExpression

instance ( Accessible a, EntryGet e a, EntryAccessible e a
         , Accessible b, EntryGet e b, EntryAccessible e b
         , Entry e) =>
    DSLExpression (DSLKnown a b) e
    where
        boxExpression = DSLC . dsl2CKnown


instance (Accessible v, EntryGet e v, Entry e) => DSLExpression (DSLCondition1 v) e where
    boxExpression = DSLC . dsl2CCond1

instance ( Accessible a, EntryGet e a, EntryAccessible e a
         , Accessible b, EntryGet e b, EntryAccessible e b
         , Accessible v, EntryGet e v
         , Entry e ) =>
    DSLExpression (DSLKnownCond1 a b v) e
    where
        boxExpression (DSLKnownCond1 known cond) = DSLC $ DSLKnownCondContainer1 k c
                                                where k = dsl2CKnown known
                                                      c = dsl2CCond1 cond

cAtomStatement :: (EntryGet entry v, Accessible v) => DSLStatement v -> (v, entry -> Maybe v)
cAtomStatement (DSLAtomic v) = (v, getV $ varDescriptor v)
-- TODO

dsl2CKnown (DSLKnown s1@(DSLAtomic _) s2@(DSLAtomic _)) =
    DSLKnownAtomsContainer a ga b gb where (a, ga) = cAtomStatement s1
                                           (b, gb) = cAtomStatement s2

dsl2CKnown (DSLKnown s1@(DSLAtomic _) s2@(DSLConstraint f)) =
    DSLKnownConstraintContainer a ga f gv where (a, ga) = cAtomStatement s1
                                                gv = getV $ varDescriptor (undefined :: v)

cCond1 vd f = DSLCondContainer1 f (getV vd) vd

dsl2CCond1 :: (EntryGet entry v, Accessible v) => DSLCondition1 v -> DSLCondContainer1 entry
dsl2CCond1 (DSLCondition1 f) = cCond1 (varDescriptor (undefined::v)) f

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --



