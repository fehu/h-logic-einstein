
module LogicProblem.Solver.History (

  SolveHistoryEntry(..)
, SolveHistory


) where

import LogicProblem.Solver.Env
import LogicProblem.Solver.Def
import LogicProblem.Solver.SDef
import LogicProblem.Solver.Apply
import LogicProblem.Solver.Hypotheses

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data SolveHistoryEntry rule e = SHistEntry (SolveInnerResult rule e) [ApplyRsEither (rule e) e]
                              | SHypApply  (rule e) (Hypothesis e) (HypothesesAlt e)
                              | SUnapply   (rule e) (Hypothesis e) [(rule e, RApplyResult (Value e))]

type SolveHistory rule e = [SolveHistoryEntry rule e]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
