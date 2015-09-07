{-# LANGUAGE MultiParamTypeClasses
            , FlexibleInstances
          #-}

module LogicProblem.Lang (

  KnownFacts
, Rule(..)

, (<==>)
, (|?>)
, (<?|)
, (!?)

, (-:)
, (|::)

, RuleKnown(..)
, RuleKnownConstraint(..)
, RuleCondition1(..)
--, RuleCondition2(..)
, RuleKnownCond1(..)
--, RuleKnownCond2(..)

) where

import LogicProblem.Rule

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

(<==>) = RuleKnown
(!?)   = RuleKnownConstraint

k |?> f  = RuleKnownCond1 k (RuleCondition1 f) False
k <?| f  = RuleKnownCond1 k (RuleCondition1 f) True

name -: expr = Rule name Nothing (boxExpression expr)
rule |:: descr = rule { ruleDescription = Just descr }

infix 3 -:
infix 2 |::

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --





-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data RuleKnown a b = RuleKnown a b
data RuleKnownConstraint a v = RuleKnownConstraint a (Maybe v -> Bool)

data RuleCondition1 v     = RuleCondition1 (Maybe v -> Maybe v -> Bool)
--data RuleCondition2 v1 v2 = RuleCondition2 ((v1,v2) -> (v1,v2) -> Bool) -- TODO

data RuleKnownCond1 a b v     = RuleKnownCond1 (RuleKnown a b) (RuleCondition1 v) Bool
--data RuleKnownCond2 a b v1 v2 = RuleKnownCond2 (RuleKnown a b) (RuleCondition2 v1 v2)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Rule e = Rule { ruleName'       :: String
                   , ruleDescription :: Maybe String
                   , ruleDef         :: RuleContainer e
                   }
                   deriving Show

instance Eq (Rule e) where r1 == r2 = ruleName' r1 == ruleName' r2

--instance Show (Rule e) where show = show . ruleName'

instance RuleDefinition Rule e where
    getRule Rule{ruleDef = (RuleC c)} = getRule' c
    ruleName = ruleName'


type KnownFacts e = [Rule e]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


