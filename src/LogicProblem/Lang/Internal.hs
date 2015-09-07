{-# LANGUAGE MultiParamTypeClasses
           , ExistentialQuantification
           , FlexibleInstances
         #-}

module LogicProblem.Lang.Internal (

) where

import Data.Maybe (maybeToList)

import LogicProblem.Rule
import LogicProblem.Lang
import LogicProblem.Solver.Env
import LogicProblem.Solver.Def


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Containers



data KnownRuleContainer entry = forall a b. ( Eq a, Show a, AccessibleEntry entry a
                                            , Eq b, Show b, AccessibleEntry entry b) =>
                                  KnownAtomsContainer a b

                              | forall a b. ( Eq a, AccessibleEntry entry a, Show a
                                            , Eq b, AccessibleEntry entry b) =>
                                  KnownConstraintContainer a (Maybe b -> Bool)
                                                             (AccessibleDescriptor b)

instance Show (KnownRuleContainer e) where
    show (KnownAtomsContainer      a b)    = show a ++ " <==> " ++ show b
    show (KnownConstraintContainer a _ vd) = show a ++ " !? " ++ show vd


applyKC' eL eR (a, ga) (b, gb) =
    case (left, right) of (Just True,  Nothing  )  -> RImplies  [implR] pr
                          (Nothing,    Just True)  -> RImplies  [implL] pr
                          (Just True,  Just True)  -> RConfirm  pr
                          (Just False, Just False) -> RBroken   pr
                          (Nothing,    Nothing)    -> RPossible [implL, implR]
                          _                        -> RBroken   pr
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

getV' v = getV (varDescriptor v)

applyKC1 :: (Entry e) => e -> KnownRuleContainer e -> RApplyResult (Value e)
applyKC1 e (KnownAtomsContainer a b) = applyKC' e e (a, getV' a)
                                                    (b, getV' b)

applyKC1 e (KnownConstraintContainer a f vd) =
    if f . gv $ e then case fmap (== a) (ga e) of Just True  -> RConfirm pr
                                                  Just False -> RBroken  pr
                                                  _          -> RImplies [pi] pr
                  else RPossible [pi]
    where ga = getV (varDescriptor a)
          gv = getV vd
          p  = (getId e, mbValList $ ga e)
          pC = (getId e, mbValList $ gv e)
          pi = (getId e, [Value a])
          pr = [p, pC]

applyKC2 :: (Entry e) => e -> e -> KnownRuleContainer e -> RApplyResult (Value e)
applyKC2 e1 e2 (KnownAtomsContainer a b) = applyKC' e1 e2 (a, getV' a) (b, getV' b)

instance (Entry e) => RuleInner KnownRuleContainer e where
    getRule' kc = RApply1 (`applyKC1` kc)


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data CondRuleContainer1 entry = forall v. (AccessibleEntry entry v) =>
    CondContainer1 (Maybe v -> Maybe v -> Bool) (AccessibleDescriptor v)

instance Show (CondRuleContainer1 e) where
    show (CondContainer1 _ (AccessibleDescriptor v)) = "Condition over " ++ show v

instance (Entry e) => RuleInner CondRuleContainer1 e where
    getRule' c = RApply2 (applyCC1 c)

applyCC1 c e1 e2 | testCCond1 c e1 e2 = RConfirm $ p e1 e2
                 | otherwise          = RBroken  $ p e1 e2
    where p e1 e2 = map (\e -> (getId e, [])) [e1, e2]

testCCond1 :: CondRuleContainer1 e -> e -> e -> Bool
testCCond1 (CondContainer1 f vd) e1 e2 = f (getV vd e1) (getV vd e2)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data KnownCondRuleContainer1 entry = KnownCondContainer1 (KnownRuleContainer entry)
                                                         (CondRuleContainer1 entry)
                                                         Bool

instance Show (KnownCondRuleContainer1 e) where
    show (KnownCondContainer1 known cond flipped) = "[" ++ show known ++ ", " ++ show cond ++ "]"

instance (Entry e) => RuleInner KnownCondRuleContainer1 e where
    getRule' (KnownCondContainer1 kc cc flipped) = RApply2 apply
        where apply e1 e2 | isSuccess known && isSuccess cond = known
                          | isFailure cond                    = cond
                          | otherwise                         = known
                  where known = applyKC2 e1 e2 kc
                        applyCC1' | flipped   = flip a
                                  | otherwise = a
                            where a = applyCC1 cc
                        cond  = applyCC1' e1 e2


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- instance RuleExpression

instance ( Accessible a, AccessibleEntry e a
         , Accessible b, AccessibleEntry e b
         , Entry e) =>
    RuleExpression (RuleKnown a b) e
    where
        boxExpression = RuleC . dsl2CKnown


instance (Accessible v, AccessibleEntry e v, Entry e) => RuleExpression (RuleCondition1 v) e where
    boxExpression = RuleC . dsl2CCond1


instance ( AccessibleEntry e a
         , AccessibleEntry e b
         , AccessibleEntry e v
         , Entry e ) =>
    RuleExpression (RuleKnownCond1 a b v) e
    where
        boxExpression (RuleKnownCond1 known cond flipped) =
            RuleC $ KnownCondContainer1 k c flipped
            where k = dsl2CKnown known
                  c = dsl2CCond1 cond

dsl2CKnown (RuleKnown a b) = KnownAtomsContainer a b



instance ( Accessible a, AccessibleEntry e a
         , Accessible v, AccessibleEntry e v
         , Entry e) =>
    RuleExpression (RuleKnownConstraint a v) e
    where
        boxExpression (RuleKnownConstraint a f) = RuleC $
            KnownConstraintContainer a f vd where vd = varDescriptor (undefined :: v)

cCond1 vd f = CondContainer1 f vd

dsl2CCond1 :: (AccessibleEntry entry v, Accessible v) => RuleCondition1 v -> CondRuleContainer1 entry
dsl2CCond1 (RuleCondition1 f) = cCond1 (varDescriptor (undefined::v)) f

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

