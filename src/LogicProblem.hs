
module LogicProblem (

  Id(..)
, IdRepr(..)

, Accessible(..)
, AccessibleDescriptor(..)

, Entry(..)
, AccessibleEntry(..)

, Rule(..)
, KnownFacts

, (<==>)
, (|?>)
, (<?|)
, (!?)

, (-:)
, (|::)

, ETable
, newETable

, ExecContext
, newExecContext

, applyRules
, solveProblem

, showHistory

) where


import LogicProblem.Lang
import LogicProblem.Lang.Internal
import LogicProblem.Solver
import LogicProblem.Solver.Env
import LogicProblem.Solver.Context
import LogicProblem.Solver.Apply
import LogicProblem.Solver.Show

