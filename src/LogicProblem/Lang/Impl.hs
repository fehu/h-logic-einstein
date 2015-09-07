{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification #-}

-- FlexibleInstances, UndecidableInstances,

module LogicProblem.Lang.Impl (

(<==>)

) where

import LogicProblem.Lang
import LogicProblem.Lang.Internal
import LogicProblem.Rule
import LogicProblem.Solver.Env


data RuleAtom e = forall a. (AccessibleEntry e a) => RuleAtom a

class RuleAtoms e as where getRuleAtoms :: as -> e -> [RuleAtom e]

--instance RuleInner RuleContainer e where

instance (AccessibleEntry e v)
 => RuleAtoms e v where
        getRuleAtoms v _ = [RuleAtom v]


(<==>) :: (RuleAtoms e as, RuleAtoms e bs) =>
    as -> bs -> e -> [RuleContainer e]

as <==> bs = \e -> do RuleAtom a <- getRuleAtoms as e
                      RuleAtom b <- getRuleAtoms bs e
                      return . boxExpression $ RuleKnown a b



