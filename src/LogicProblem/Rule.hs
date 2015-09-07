module LogicProblem.Rule (

  RuleDefinition(..)
, RuleContainer(..)
, RuleExpression(..)

, RuleInner(..)

) where

import LogicProblem.Solver.Def

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class RuleDefinition r e where
    getRule  :: r e -> RApply e
    ruleName :: r e -> String

class RuleInner r e where
    getRule' :: r e -> RApply e


data RuleContainer e = forall c. (RuleInner c e, Show (c e)) => RuleC (c e)

instance Show (RuleContainer e) where
    show (RuleC c) = show c


class RuleExpression expr e where
    boxExpression :: expr -> RuleContainer e

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


