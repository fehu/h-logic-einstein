
module ProblemStatement.Exec (

  SApplyResult(..)

) where

import ProblemStatement.Statement


type SEntry v = (Id, [v])

data SApplyResult v = SImplies  { what :: [SEntry v], reason :: [SEntry v] }
                    | SBroken   [SEntry v]
--                    | SMultiple [SEntry v]
                    | SConfirm  [SEntry v]
                    | SEmpty    [SEntry v]
                    | SPossible [SEntry v]
