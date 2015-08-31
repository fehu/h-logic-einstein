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

updREntry t = updT (`getEntry` t) t

applyARule r t =
    case executeRule r t of res@(RuleApplies _ (RImplies x _)) -> (updREntry t x, res)
                            res                                -> (t, res)

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

applyRules'' t (RuleApplies _ (RImplies r _) : rs) = applyRules'' (updREntry t r) rs
applyRules'' t  [] = t

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
