
module LogicProblem.Solver.SDef (

  SolveResult(..)
, SolveInnerResult(..)

, solveResult
, isSolved


) where

import LogicProblem.Solver.Def
import LogicProblem.Solver.Hypotheses

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data SolveInnerResult r e = NewHypotheses    (HypothesesLevel r e)
                          | FallbackRequired (RuleResult (r e) e)
                          | CanDoNothing
                          | Stopped

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data SolveResult r e = SolveSuccess (SolveInnerResult r e)
                     | SolveFailure (SolveInnerResult r e)

solveResult (SolveSuccess res) = res
solveResult (SolveFailure res) = res

isSolved (SolveSuccess _) = True
isSolved _ = False

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

