{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
         #-}

module Problem.DSL (

 KnownFacts
, Rule(..)

, (<==>)
, (|?>)

, (-:)
, (|::)

) where

import Problem.DSL.Struct
import Problem.Exec

x <==> y = DSLKnown (DSLAtomic x) (DSLAtomic y)
k |?> f  = DSLKnownCond1 k (DSLCondition1 f)


data Rule e = Rule { ruleName        :: String
                   , ruleDescription :: Maybe String
                   , ruleDef         :: DSLContainerContainer e
                   }
                   deriving Show

type KnownFacts e = [Rule e]

instance RuleDefinition Rule e where
    applyRule Rule{ruleDef = (DSLC c)} = applyC c

--    applyRule r e1 e2 = case ruleDef r of DSLC c -> applyC e1 e2 c

name -: expr = Rule name Nothing (boxExpression expr)
rule |:: descr = rule { ruleDescription = Just descr }

infix 3 -:
infix 2 |::


--dslToInternal :: KnownFacts e -> [DSLContainerContainer e]
