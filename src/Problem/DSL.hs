module Problem.DSL (

 KnownFacts

, (<==>)
, (|?>)

, (-:)
, (|::)

) where

import Problem.DSL.Struct

x <==> y = DSLKnown (DSLAtomic x) (DSLAtomic y)
k |?> f  = DSLKnownCond1 k (DSLCondition1 f)


data Rule e = Rule { ruleName        :: String
                   , ruleDescription :: Maybe String
                   , ruleDef         :: DSLContainerContainer e
                   }
                   deriving Show

type KnownFacts e = [Rule e]

name -: expr = Rule name Nothing (boxExpression expr)
rule |:: descr = rule { ruleDescription = Just descr }

infix 3 -:
infix 2 |::


--dslToInternal :: KnownFacts e -> [DSLContainerContainer e]
