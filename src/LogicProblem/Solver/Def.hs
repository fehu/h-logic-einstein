module LogicProblem.Solver.Def (

  RApply(..)
, REntry
, RApplyResult(..)

, isSuccess
, isFailure
, isUndetermined
, getResultEntries

, RuleResult(..)

) where

import LogicProblem.Solver.Env

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type ApplyRule1 e = e -> RApplyResult (Value e)


data RApply e = RApply1 (ApplyRule1 e)
              | RApply2 (e -> ApplyRule1 e)


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type REntry v = (Id, [v])

data RApplyResult v = RImplies  { what   :: [REntry v]
                                , reason :: [REntry v]
                                }
                    | RBroken   [REntry v]
                    | RConfirm  [REntry v]
                    | RPossible [REntry v]
                    deriving Show


isSuccess        :: RApplyResult v -> Bool
isFailure        :: RApplyResult v -> Bool
isUndetermined   :: RApplyResult v -> Bool
getResultEntries :: RApplyResult v -> [REntry v]

isSuccess (RImplies _ _) = True
isSuccess (RConfirm _)   = True
isSuccess _              = False

isFailure (RBroken _) = True
isFailure _           = False

isUndetermined (RPossible _) = True
isUndetermined _             = False

getResultEntries RImplies {what = w} = w
getResultEntries (RBroken es)        = es
getResultEntries (RConfirm es)       = es
getResultEntries (RPossible es)      = es

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data RuleResult r e = RuleContradicts r [RApplyResult (Value e)]
                    | RuleApplies     r (RApplyResult (Value e))
                    | RuleMultiple    r [RApplyResult (Value e)]
                    | RuleUnmatched   r [RApplyResult (Value e)]
                    deriving Show

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --



