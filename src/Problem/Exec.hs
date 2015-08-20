
module Problem.Exec (

  SApplyResult(..)

) where

import Problem.Statement


type SEntry v = (Id, [v])

data SApplyResult v = SImplies  { what   :: [SEntry v]
                                , reason :: [SEntry v]
                                }
                    | SBroken   [SEntry v]
                    | SConfirm  [SEntry v]
                    | SEmpty    [SEntry v]
                    | SPossible [SEntry v]
