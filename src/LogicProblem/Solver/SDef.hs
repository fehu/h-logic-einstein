module LogicProblem.Solver.SDef (

  SolveResult(..)
, SolveInnerResult(..)

, solveResult
, hasFinished


) where

import LogicProblem.Solver.Def
import LogicProblem.Solver.Hypotheses

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data SolveInnerResult r e = NewHypotheses    (HypothesesLevel r e)
                          | FallbackRequired [RuleResult (r e) e]
                          | RulesApplied     [RuleResult (r e) e]
                          | CanDoNothing
                          | Stopped

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data SolveResult r e = SolveFinish  (SolveInnerResult r e)
                     | SolveFailure (SolveInnerResult r e)

solveResult (SolveFinish res) = res
solveResult (SolveFailure res) = res

hasFinished (SolveFinish _) = True
hasFinished  _ = False

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

