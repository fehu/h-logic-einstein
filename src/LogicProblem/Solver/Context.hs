module LogicProblem.Solver.Context (

  ContextTable(..)
, ContextHypotheses(..)
, SolveContext(..)

, ExecContext(..)
, newExecContext

) where

import LogicProblem.Solver.Hypotheses
import LogicProblem.Solver.Env

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class ContextTable c e where
    contextTable    :: c e -> ETable e
    updContextTable :: c e -> ETable e -> c e

class ContextHypotheses c e r where
    contextHypotheses    :: c e -> [HypothesesLevel r e]
    updContextHypotheses :: c e -> [HypothesesLevel r e] -> c e

class (ContextTable c e, ContextHypotheses c e r) => SolveContext c e r where
    solveContext :: ETable e -> [HypothesesLevel r e] -> c e


newtype ExecContext r e = ExecContext (ETable e, [HypothesesLevel r e])

newExecContext t = ExecContext (t, [])

instance ContextTable (ExecContext r) e where
    contextTable (ExecContext (t, _)) = t
    updContextTable (ExecContext (_, h)) t = ExecContext (t, h)

instance ContextHypotheses (ExecContext r) e r where
    contextHypotheses (ExecContext (_, h)) = h
    updContextHypotheses (ExecContext (t, _)) h = ExecContext (t, h)

instance SolveContext (ExecContext r) e r where
    solveContext = curry ExecContext

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

