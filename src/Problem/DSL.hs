{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
         #-}

module Problem.DSL (

 KnownFacts
, Rule(..)

, (<==>)
, (|?>)
, (!?)

, (-:)
, (|::)

) where

import Problem.DSL.Struct
import Problem.Exec

x <==> y = DSLKnown (DSLAtomic x) (DSLAtomic y)
x !? c   = DSLKnown (DSLAtomic x) (DSLConstraint c)

k |?> f  = DSLKnownCond1 k (DSLCondition1 f)


data Rule e = Rule { ruleName        :: String
                   , ruleDescription :: Maybe String
                   , ruleDef         :: DSLContainerContainer e
                   }

instance Show (Rule e) where show = show . ruleName

type KnownFacts e = [Rule e]

instance RuleDefinition Rule e where
    extractRule Rule{ruleDef = (DSLC c)} = applyC c


name -: expr = Rule name Nothing (boxExpression expr)
rule |:: descr = rule { ruleDescription = Just descr }

infix 3 -:
infix 2 |::


