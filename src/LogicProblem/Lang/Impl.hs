{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification
        , TypeFamilies
        , AllowAmbiguousTypes
        , IncoherentInstances
 #-} -- , AllowAmbiguousTypes

-- FlexibleInstances, UndecidableInstances,

module LogicProblem.Lang.Impl (

  (<==>)
, (&)


, RuleAnd(..)

) where

import LogicProblem.Lang
import LogicProblem.Lang.Internal
import LogicProblem.Rule
import LogicProblem.Solver.Env

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data RuleAtom e = forall a. (AccessibleEntry e a) => RuleAtom a

class RuleAtoms  e as     where getRuleAtoms  ::        as -> e -> [RuleAtom e]
class RuleAtoms' e as tpe where getRuleAtoms' :: tpe -> as -> e -> [RuleAtom e]

data Atom
data FAnd

type family ShowPred a where
  ShowPred (RuleAtom e) = Atom
  ShowPred (RuleAnd e)  = FAnd

instance (ShowPred a ~ flag, RuleAtoms' e a flag) => RuleAtoms e a where
    getRuleAtoms = getRuleAtoms' (undefined::flag)

instance RuleAtoms' e a flag where
  getRuleAtoms' _ _ = undefined

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --



instance (AccessibleEntry e v)
 => RuleAtoms' e v Atom where
        getRuleAtoms' _ v _ = [RuleAtom v]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data RuleAnd e = RuleAnd [RuleAtom e]

--instance RuleAtoms e (e -> RuleAnd e) where
--    getRuleAtoms f e = let (RuleAnd as) = f e
--                     in as

(&) :: (RuleAtoms e as , RuleAtoms e bs) =>
    as -> bs -> e -> RuleAnd e

(&) as bs e = RuleAnd $ getA ++ getB
    where getA = getRuleAtoms as e
          getB = getRuleAtoms  bs e


instance RuleAtoms' e (e -> RuleAnd e) FAnd where
    getRuleAtoms' _ f e = let (RuleAnd as) = f e
                        in as

--(&) :: (RuleAtoms' e as FAnd, RuleAtoms' e bs FAnd) =>
--    as -> bs -> e -> RuleAnd e
--
--(&) as bs e = RuleAnd $ getA ++ getB
--    where getA = getRuleAtoms' FAnd as e
--          getB = getRuleAtoms' FAnd bs e

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


(<==>) :: (RuleAtoms e as, RuleAtoms e bs) =>
    as -> bs -> e -> [RuleContainer e]

as <==> bs = \e -> do RuleAtom a <- getRuleAtoms as e
                      RuleAtom b <- getRuleAtoms bs e
                      return . boxExpression $ RuleKnown a b



