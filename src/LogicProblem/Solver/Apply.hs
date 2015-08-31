{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , UndecidableInstances
         #-}

module LogicProblem.Solver.Apply (

  executeRule
, applyARule
, applyRules
, applyRules''

, ApplyRsEither
, ApplyRsSuccess
, ApplyRsFailure

, EntryValExt(..)

, ruleImplies
, ruleContradicts
, ruleMultiple
, ruleResults

, updREntry

) where

import qualified Data.Map as M

import Data.Maybe (fromMaybe)
import Data.List  (partition)

import LogicProblem.Rule
import LogicProblem.Solver.Def
import LogicProblem.Solver.Env

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

applyR :: RApply e -> ETable e -> [RApplyResult (Value e)]

applyR (RApply1 f) (ETable mp) = map (f . snd) (M.assocs mp)

applyR (RApply2 f) (ETable mp) = do (i1, e1) <- ies
                                    let ids1 = replicate (length ies) i1
                                    (i2, e2) <- ies
                                    return $ f e1 e2
                              where ies = M.assocs mp


executeRule r t = apply
    where res = applyR (getRule r) t
          broken = filter isFailure res
          imply  = filter isSuccess res
          apply | length broken == length res = RuleContradicts r broken
                | length imply == 1           = RuleApplies     r (head imply)
                | not . null $ imply          = RuleMultiple    r imply
                | otherwise                   = RuleUnmatched   r res

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

ruleContradicts (RuleContradicts _ _) = True
ruleContradicts _ = False

ruleImplies (RuleApplies _ (RImplies _ _)) = True
ruleImplies _ = False

ruleMultiple (RuleMultiple _ _) = True
ruleMultiple _ = False

ruleResults (RuleContradicts _ rs) = rs
ruleResults (RuleApplies _ rs)     = [rs]
ruleResults (RuleMultiple _ rs)    = rs
ruleResults (RuleUnmatched _ rs)   = rs

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class (Entry e) => EntryValExt e where
    setValue   :: Value e -> e -> Maybe e
    clearValue :: Value e -> e -> Maybe e

instance (Entry e) => EntryValExt e where
    setValue   (Value v) = flip setV v
    clearValue (Value v) = clearV $ varDescriptor v


updT getE t ((id, vs):entries) = updT getE t' entries
                       where t' = setEntry e' t
                             e' = setVs (getE id) vs
updT _ t [] = t


setVs e (v:vs) = let mbE = setValue v e
                 in setVs (fromMaybe e mbE) vs
setVs e [] = e


--presentInTable t vs = all (notSet' vs) $ listEntries t
--
--notSet :: e -> Value e -> Bool
--notSet e (Value v) = maybe True (/= v) $ getV (varDescriptor v) e
--
--notSet' vs e = all (notSet e) (concatMap snd vs)


--partitionAlreadyPresent t vs = do e <- listEntries t
--                                  partition (notSet e) vs
--
--notSet :: e -> Value e -> Bool
--notSet e (Value v) = maybe True (/= v) $ getV (varDescriptor v) e

partitionAlreadyPresent t = partition (isAnySet t)

isAnySet t (_, vs) = any (isSet' t) vs

isSet :: Value e -> e -> Bool
isSet (Value v) e = modifiable v
                 && maybe False (== v) (getV (varDescriptor v) e)
isSet' t v  = any (isSet v) $ listEntries t

updREntry t = updT (`getEntry` t) t


applyARule r t =
    case executeRule r t of res@(RuleApplies _ (RImplies x _))
                                | isPresentInTable t x-> (t, RuleUnmatched r $ results t x)
                                | otherwise          -> (updREntry t x, res)
                            res                      -> (t, res)
    where presentInTable = partitionAlreadyPresent
          isPresentInTable t vs = not . null . fst $ presentInTable t vs
          results t vs = let (alreadySet, unSet) = presentInTable t vs
                         in [RBroken alreadySet, RPossible unSet]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type ApplyRsSuccess r e = [RuleResult r e]
type ApplyRsFailure r e = (RuleResult r e, [RuleResult r e])

type ApplyRsEither r e = Either (ApplyRsSuccess r e) (ApplyRsFailure r e)

--instance Accessible (Value e)
--
--instance AccessibleEntry e (Value e) where


applyRules :: (RuleDefinition r e, EntryValExt e) =>
                        [r e] -> ETable e -> (ETable e, ApplyRsEither (r e) e)
applyRules rs t = res
    where res = applyRules' rs t (Left [])

applyRules' (r:rs) t (Left acc) = let (t', res) = applyARule r t
                         in if ruleContradicts res
                            then (t, Right (res, acc))
                            else applyRules' rs t' (Left (res:acc))
applyRules' [] t acc = (t, acc)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

applyRules'' t (RuleApplies _ (RImplies x _) : rs) = applyRules'' (updREntry t x) rs
applyRules'' t  [] = t

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
