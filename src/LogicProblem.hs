
module LogicProblem (

  Id(..)
, IdRepr(..)

, Accessible(..)
, AccessibleDescriptor(..)

, Entry(..)
, AccessibleEntry(..)

, rules
, AtomicRule(..)
, Rule(..)
, KnownFacts

, (<==>)
, (|?>)
, (<?|)
--, (!?)

, (-:)
, (|::)

, ETable
, newETable

, ExecContext
, newExecContext

, solveProblem

, showHistory

) where


import LogicProblem.Lang
import LogicProblem.Lang.Internal
import LogicProblem.Lang.Impl
import LogicProblem.Solver
import LogicProblem.Solver.Env
import LogicProblem.Solver.Context
import LogicProblem.Solver.Apply
import LogicProblem.Solver.Show

