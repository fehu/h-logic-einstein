{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , UndecidableInstances
         #-}

module LogicProblem.Solver.Apply (

  executeRule
, executeRules
, applyRules''

, partitionAlreadyPresent

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

executeRules rs t = map (`executeRule` t) rs

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


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

partitionAlreadyPresent t = partition (isAnySet t)

isAnySet t (_, vs) = any (isSet' t) vs

isSet :: Value e -> e -> Bool
isSet (Value v) e = modifiable v
                 && maybe False (== v) (getV (varDescriptor v) e)
isSet' t v  = any (isSet v) $ listEntries t


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

updREntry t = updT (`getEntry` t) t

applyRules'' t (RuleApplies _ (RImplies x _) : rs) = applyRules'' (updREntry t x) rs
applyRules'' t  [] = t

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

