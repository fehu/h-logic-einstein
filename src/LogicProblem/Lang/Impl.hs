{-# LANGUAGE MultiParamTypeClasses
           , ExistentialQuantification
           , AllowAmbiguousTypes
--           , FunctionalDependencies
--           , TypeFamilies
--           , IncoherentInstances
--           , OverlappingInstances
        #-}

module LogicProblem.Lang.Impl (

  (&)
, (!?)
, (<==>)

, (|?>)
, (<?|)

, (-:)
, (|::)


) where

import LogicProblem.Lang
import LogicProblem.Lang.Internal
import LogicProblem.Rule
import LogicProblem.Solver.Env


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data AtomPair e = AtomPair (RuleAtom e) (RuleAtom e)
                | AtomPairCondCompiled [RuleContainer e]

--getPairLeft (AtomPair (RuleAtom l) _) = l

(-:) :: String -> (e -> [AtomPair e]) -> Rule e

--(-:) = undefined

--name -: (AtomPair l r) = Rule name Nothing (boxExpression $ RuleKnown l r)

name -: fc = Rule name Nothing c
    where c = case fc undefined of [AtomPairCondCompiled cs] -> cs
                                   pairs -> do AtomPair (RuleAtom l) (RuleAtom r) <- pairs
                                               return . boxExpression $ RuleKnown l r
--    where c = do AtomPairCondCompiled
--                 c'

--name -: fpairs = Rule name Nothing c
--    where c = do AtomPair (RuleAtom l) (RuleAtom r) <- fpairs undefined
--                 return . boxExpression $ RuleKnown l r

rule |:: descr = rule { ruleDescription = Just descr }

infix 3 -:
infix 2 |::

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data RuleAtom e = forall a. (AccessibleEntry e a) => RuleAtom a

class RuleAtoms e a where getRuleAtoms :: a -> e -> [RuleAtom e]

--data RuleAtomsContainer e = forall a. (RuleAtoms e a) => RuleAtoms a




--instance (ShowPred a flag, RuleAtoms' e a flag) => RuleAtoms e a where
--    getRuleAtoms = getRuleAtoms' (undefined::flag)

--instance RuleAtoms' e a flag where
--  getRuleAtoms' _ _ _ = error "getRuleAtoms'"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

instance (AccessibleEntry e v)
 => RuleAtoms e v where
        getRuleAtoms v _ = [RuleAtom v]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data RuleAnd e = RuleAnd [RuleAtom e]

(&) :: (RuleAtoms e as , RuleAtoms e bs) =>
    as -> bs -> e -> RuleAnd e

(&) as bs e = RuleAnd $ getA ++ getB
    where getA = getRuleAtoms as e
          getB = getRuleAtoms  bs e


--instance RuleAtoms' e (e -> RuleAnd e) FAnd where
--    getRuleAtoms' _ f e = let (RuleAnd as) = f e
--                        in as

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

(<==>) :: (RuleAtoms e as, RuleAtoms e bs) =>
    as -> bs -> e -> [AtomPair e]

as <==> bs = \e -> do a <- getRuleAtoms as e
                      b <- getRuleAtoms bs e
                      [ AtomPair a b ]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type KCond e v = (e -> [AtomPair e]) -> (Maybe v -> Maybe v -> Bool) -> e -> [AtomPair e]

kcond :: (AccessibleEntry e v) => Bool -> KCond e v

kcond flip' fa f e = do let ps = fa e
                        p <- ps
                        return $ AtomPairCondCompiled [rKCond' p f flip']

rKCond' :: (AccessibleEntry e v) => AtomPair e -> (Maybe v -> Maybe v -> Bool) -> Bool -> RuleContainer e
rKCond' (AtomPair (RuleAtom l) (RuleAtom r)) = rKCond l r

rKCond :: (AccessibleEntry e a, AccessibleEntry e b, AccessibleEntry e v) =>
    a -> b -> (Maybe v -> Maybe v -> Bool) -> Bool -> RuleContainer e

rKCond a b f flip' = boxExpression $ RuleKnownCond1 (RuleKnown a b) (RuleCondition1 f) flip'



(|?>) :: (AccessibleEntry e v) => KCond e v
(<?|) :: (AccessibleEntry e v) => KCond e v

(|?>) = kcond False
(<?|) = kcond True

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

(!?) :: (RuleAtoms e as, AccessibleEntry e v) => as -> (Maybe v -> Bool) -> e -> [AtomPair e]

(!?) as f e = do RuleAtom a <- getRuleAtoms as e
                 let x = AtomPairCondCompiled [ boxExpression $ RuleKnownConstraint a f ]
                 [ x ]

