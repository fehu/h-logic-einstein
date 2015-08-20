
module Problem.Exec (

  SApplyResult(..)

, isSuccess
, isFailure
, isUndetermined
, getResultEntries

) where

import qualified Data.Map as M (Map, fromList)

import Problem.Statement


type SEntry v = (Id, [v])

data SApplyResult v = SImplies  { what   :: [SEntry v]
                                , reason :: [SEntry v]
                                }
                    | SBroken   [SEntry v]
                    | SConfirm  [SEntry v]
                    | SEmpty    [SEntry v]
                    | SPossible [SEntry v]


isSuccess        :: SApplyResult v -> Bool
isFailure        :: SApplyResult v -> Bool
isUndetermined   :: SApplyResult v -> Bool
getResultEntries :: SApplyResult v -> [SEntry v]

isSuccess (SImplies _ _) = True
isSuccess (SConfirm _)   = True
isSuccess _              = False

isFailure (SBroken _) = True
isFailure _           = False

isUndetermined (SEmpty _)    = True
isUndetermined (SPossible _) = True
isUndetermined _             = False

getResultEntries SImplies {what = w} = w
getResultEntries (SBroken es)        = es
getResultEntries (SConfirm es)       = es
getResultEntries (SEmpty es)         = es
getResultEntries (SPossible es)      = es


newtype ETable e = ETable (M.Map Id e)

newETable :: (a -> (Id, e)) -> [a] -> ETable e
newETable f as = ETable $ M.fromList (map f as)


