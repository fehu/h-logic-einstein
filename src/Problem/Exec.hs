{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-} -- , FlexibleContexts


module Problem.Exec (

  SApply(..)
, SApplyResult(..)

, isSuccess
, isFailure
, isUndetermined
, getResultEntries

, ETable
, newETable

, EntryValExt(..)
, Value(..)
, RuleDefinition(..)

, applyS
, applyARule
, applyRules

, showHistory

, solveProblem
, ExecContext
, newExecContext

) where

import qualified Data.Map as M

import Data.Maybe    (fromMaybe, maybeToList)
import Data.List     (intercalate)
--import Data.Monoid
import Control.Monad (mzero)

import Problem.Statement


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


type SApply1 e = e -> SApplyResult (Value e)

data SApply e = SApply1 (SApply1 e)
              | SApply2 (e -> SApply1 e)


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type SEntry v = (Id, [v])

data SApplyResult v = SImplies  { what   :: [SEntry v]
                                , reason :: [SEntry v]
                                }
                    | SBroken   [SEntry v]
                    | SConfirm  [SEntry v]
                    | SEmpty    [SEntry v]
--                    | SPossible [SEntry v]
                    deriving Show


isSuccess        :: SApplyResult v -> Bool
isFailure        :: SApplyResult v -> Bool
isUndetermined   :: SApplyResult v -> Bool
getResultEntries :: SApplyResult v -> [SEntry v]

isSuccess (SImplies _ _) = True
isSuccess (SConfirm _)   = True
isSuccess _              = False

isFailure (SBroken _) = True
isFailure _           = False

isUndetermined (SEmpty _)    = True
--isUndetermined (SPossible _) = True
isUndetermined _             = False

getResultEntries SImplies {what = w} = w
getResultEntries (SBroken es)        = es
getResultEntries (SConfirm es)       = es
getResultEntries (SEmpty es)         = es
--getResultEntries (SPossible es)      = es


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


newtype ETable e = ETable (M.Map Id e)

newETable :: (a -> (Id, e)) -> [a] -> ETable e
newETable f as = ETable $ M.fromList (map f as)

getEntry :: Id -> ETable e -> e
getEntry id (ETable mp) = mp M.! id

setEntry :: (Entry e) => e -> ETable e -> ETable e
setEntry e (ETable mp) = ETable $ M.adjust (const e) (getId e) mp

instance (Show e) => Show (ETable e) where
    show (ETable mp) = intercalate "\n" $ map show (M.elems mp)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


class (Entry e) => EntryValExt e where
    setValue :: Value e -> e -> Maybe e

class RuleDefinition r e where
    extractRule :: r e -> SApply e



updT selE t ((id, vs):entries) = updT selE t' entries
                       where t' = setEntry e' t
                             e' = setVs (selE id) vs
updT _ t [] = t
setVs e (v:vs) = let mbE = setValue v e
                 in setVs (fromMaybe e mbE) vs
setVs e [] = e

updSEntry :: (EntryValExt e) =>  ETable e -> [SEntry (Value e)] -> ETable e
updSEntry t = updT (`getEntry` t) t

--applyARule1 rule t ((k, e):kes) acc =
--    let result = rule e
--    in applyARule1 rule t kes (result:acc)
--
--applyARule1 _ t [] acc = (t, acc)
--
--applyARule2 rule t ((k1, e1):kes1) ((k2, e2):kes2) acc =
--    let applyRes = rule e1 e2
--        result = if k1 /= k2
--            then case applyRes of res@(SImplies what _) ->
--                                        Just (updT selE t what, res)
--                                            where selE id | getId e1 == id = e1
--                                                          | getId e2 == id = e2
--                                  res -> Just (t, res)
--            else Nothing
--        t' = maybe t fst result
--        res = maybeToList $ fmap snd result
--    in applyARule2 rule t' kes1 kes2 (res ++ acc)
--
--applyARule2  _ t [] [] acc = (t, acc)

-- TODO: stop if broken
applyS :: (EntryValExt e) => SApply e -> ETable e -> [SApplyResult (Value e)]

applyS (SApply1 f) (ETable mp) = map (f . snd) (M.assocs mp)

applyS (SApply2 f) (ETable mp) = do (i1, e1) <- ies
                                    let ids1 = replicate (length ies) i1
                                    (i2, e2) <- ies
                                    return $ f e1 e2
                              where ies = M.assocs mp
--    where apply (i:is) t' acc = let ids1 = replicate (length ids) i
--                                    (t'', res) = f t' ids1 ids []
--                                in apply is t'' (res ++ acc)
--          apply [] t' acc = (t', acc)


data RuleResult r e = RuleContradicts r [SApplyResult e]
                    | RuleApplies     r (SApplyResult e)
                    | RuleMultiple    r [SApplyResult e]
                    | RuleUnmatched   r [SApplyResult e]
                    deriving Show

--instance (Show r) => Show (RuleResult r e) where
--    show (RuleContradicts r _) = "RuleContradicts " ++ show r
--    show (RuleApplies     r _) = "RuleApplies "     ++ show r
--    show (RuleMultiple    r _) = "RuleMultiple "    ++ show r
--    show (RuleUnmatched   r _) = "RuleUnmatched "   ++ show r

executeRule r t = apply
    where res = applyS (extractRule r) t
          broken = filter isFailure res
          imply  = filter isSuccess res
          apply | length broken == length res = RuleContradicts r broken
                | length imply == 1           = RuleApplies     r (head imply)
                | not . null $ imply          = RuleMultiple    r imply
                | otherwise                   = RuleUnmatched   r res

ruleContradicts (RuleContradicts _ _) = True
ruleContradicts _ = False

ruleImplies (RuleApplies _ (SImplies _ _)) = True
ruleImplies _ = False

ruleMultiple (RuleMultiple _ _) = True
ruleMultiple _ = False

ruleResults (RuleContradicts _ rs) = rs
ruleResults (RuleApplies _ rs)     = [rs]
ruleResults (RuleMultiple _ rs)    = rs
ruleResults (RuleUnmatched _ rs)   = rs

applyARule r t =
    case executeRule r t of res@(RuleApplies _ (SImplies x _)) -> (updSEntry t x, res)
                            res                                -> (t, res)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type ApplyRsSuccess r e = [RuleResult (r e) (Value e)]
type ApplyRsFailure r e =  RuleResult (r e) (Value e)

type ApplyRsEither r e = Either (ApplyRsSuccess r e) (ApplyRsFailure r e)

applyRules :: (RuleDefinition r e, EntryValExt e) =>
                            [r e] -> ETable e -> (ETable e, ApplyRsEither r e)
applyRules rs t = res
    where res = applyRules' rs t (Left [])

applyRules' (r:rs) t (Left acc) = let (t', res) = applyARule r t
                         in if ruleContradicts res
                            then (t, Right res)
                            else applyRules' rs t' (Left (res:acc))
applyRules' [] t acc = (t, acc)

showHistory :: (Show (r e)) => ApplyRsEither r e -> String

showHistory (Left success)  = str
                        where str = resStr
                              resStr = do r <- success
                                          let rS = concatMap (("\n\t\t" ++) . show) (ruleResults r)
                                          " | " ++ show r ++ rS ++ "\n\n"
showHistory (Right failure) = "!! " ++ show failure

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- Solving

data SolveResult r e = -- HadProgress ??
                       NewHypotheses    (HypothesesLevel r e)
                     | FallbackRequired (RuleResult (r e) (Value e))
                     | CanDoNothing     [RuleResult (r e) (Value e)]
                     | Stopped
                     deriving Show

newtype Hypothesis e = Hypothesis [(Id, [Value e])]    deriving Show
newtype HypothesesAlt e = HypothesesAlt [Hypothesis e] deriving Show

--instance Monoid (HypothesesAlt e) where
--    mempty = HypothesesAlt []
--    mappend (HypothesesAlt xs) (HypothesesAlt ys) = HypothesesAlt $ mappend xs ys

type Hypotheses r e = [(r e, HypothesesAlt e)]

data HypothesesLevel r e = HypothesesLevel {
      hypsInQueue :: Hypotheses r e
    , hypsFailed  :: Hypotheses r e
    , currentRule :: r e
    , currentHypQ :: HypothesesAlt e
    , currentHyp  :: Hypothesis e
    , currentHypF :: HypothesesAlt e
    }
    deriving Show

--instance (Show (r e)) => Show (HypothesesLevel r e) where
--    show hl = "* HypothesesLevel:\n" ++
--              "  - current rule: " ++ (show $ currentRule hl) ++
--              "  - current: "      ++ (show $ currentHyp hl)


class ContextTable c e where
    contextTable :: c e -> ETable e

class ContextHypotheses c e r where
    contextHypotheses :: c e -> [HypothesesLevel r e]

class (ContextTable c e, ContextHypotheses c e r) => SolveContext c e r where
    solveContext :: ETable e -> [HypothesesLevel r e] -> c e


newtype ExecContext r e = ExecContext (ETable e, [HypothesesLevel r e]) deriving Show

newExecContext t = ExecContext (t, [])

instance ContextTable (ExecContext r) e where
    contextTable (ExecContext (t, _)) = t

instance ContextHypotheses (ExecContext r) e r where
    contextHypotheses (ExecContext (_, h)) = h

instance SolveContext (ExecContext r) e r where
    solveContext = curry ExecContext

--instance (Show e) => Show (ExecContext r e) where
--    show (ExecContext (t, h)) = "Table:\n" ++ show t ++ "\n\n" ++
--                                "Hypotheses:\n" ++ show h ++ "\n"


solveProblem :: (SolveContext context e rule, EntryValExt e, RuleDefinition rule e) =>
    context e -> [rule e] -> Maybe Int  -> (context e, SolveResult rule e, [ApplyRsEither rule e])

solveProblem c rs mbMax = (solveContext t h, res, acc)
                where (t, res, acc) = solveProblem' stop (contextTable c) rs []
                      h = case res of NewHypotheses hl -> hl : contextHypotheses c
                                      _                -> contextHypotheses c
                      stop acc = maybe False (length acc ==) mbMax


newHypotheses rrs = HypothesesLevel hQueue hFailed cRule chQueue ch chFailed
    where hQueue   = tail hAlts
          (cRule, HypothesesAlt cha) = head hAlts
          ch       = head cha
          chQueue  = HypothesesAlt $ tail cha
          hFailed  = []
          chFailed = HypothesesAlt []
          hAlts = do (RuleMultiple r rs) <- rrs
                     let hyps = do (SImplies ws _) <- rs
                                   return $ Hypothesis ws
                     return (r, HypothesesAlt hyps)


-- solve by depth
solveProblem' stop t rs acc =
    case res of Left rrs | stop acc            -> (t, Stopped, acc)
                         | any ruleImplies rrs -> solveProblem' stop t' rs (res:acc)
                         | otherwise           -> (t', notImplies rrs, acc)
                Right rc                       -> (t', FallbackRequired rc, acc)
    where (t', res) = applyRules rs t
          notImplies rrs =
            let omf r = ruleMultiple r && all isImplies (ruleResults r)
                onlyMultiple = filter omf rrs
            in case onlyMultiple of [] -> CanDoNothing rrs
                                    ms -> NewHypotheses $ newHypotheses ms
          isImplies (SImplies _ _) = True
          isImplies _ = False







