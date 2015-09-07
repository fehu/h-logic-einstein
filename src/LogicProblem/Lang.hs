module LogicProblem.Lang (

  KnownFacts
, Rule(..)
, rules
, AtomicRule(..)

, RuleKnown(..)
, RuleKnownConstraint(..)
, RuleCondition1(..)
--, RuleCondition2(..)
, RuleKnownCond1(..)
--, RuleKnownCond2(..)

) where

import LogicProblem.Rule

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
                   , ruleDefs        :: [RuleContainer e]
                   }
                   deriving Show

instance Eq (Rule e) where r1 == r2 = ruleName' r1 == ruleName' r2

--instance Show (Rule e) where show = show . ruleName'

data AtomicRule e = AtomicRule { aRuleName' :: String
                               , aRuleSub   :: Maybe Char
                               , ruleDef    :: RuleContainer e
                               }
                   deriving Show

instance Eq (AtomicRule e) where r1 == r2 = aRuleName' r1 == aRuleName' r2
                                           && aRuleSub r1 == aRuleSub r2

instance RuleDefinition AtomicRule e where
    getRule AtomicRule{ruleDef = (RuleC c)} = getRule' c
    ruleName (AtomicRule nme sub _)= nme ++ maybe [] (("." ++) . show) sub

rules :: [Rule e] -> [AtomicRule e]
rules = concatMap f
    where f (Rule nme _ [r]) = [AtomicRule nme Nothing r]
          f r@(Rule _ _ [])  = error $ "Rule not defined: " ++ show r
          f (Rule nme _ rs)  = do (r, i) <- zip rs ['a'..]
                                  return $ AtomicRule nme (Just i) r

type KnownFacts e = [AtomicRule e]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


