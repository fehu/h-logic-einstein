{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}


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
import Control.Monad (mzero)

import GHC.Exts

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
--                    | SEmpty    [SEntry v]
                    | SPossible [SEntry v]
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

--isUndetermined (SEmpty _)    = True
isUndetermined (SPossible _) = True
isUndetermined _             = False

getResultEntries SImplies {what = w} = w
getResultEntries (SBroken es)        = es
getResultEntries (SConfirm es)       = es
--getResultEntries (SEmpty es)         = es
getResultEntries (SPossible es)      = es


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

class (Show (r e)) => RuleDefinition r e where
    extractRule :: r e -> SApply e
    ruleName    :: r e -> String



updT getE t ((id, vs):entries) = updT getE t' entries
                       where t' = setEntry e' t
                             e' = setVs (getE id) vs
updT _ t [] = t
setVs e (v:vs) = let mbE = setValue v e
                 in setVs (fromMaybe e mbE) vs
setVs e [] = e

updSEntry :: (EntryValExt e) =>  ETable e -> [SEntry (Value e)] -> ETable e
updSEntry t = updT (`getEntry` t) t


applyS :: (EntryValExt e) => SApply e -> ETable e -> [SApplyResult (Value e)]

applyS (SApply1 f) (ETable mp) = map (f . snd) (M.assocs mp)

applyS (SApply2 f) (ETable mp) = do (i1, e1) <- ies
                                    let ids1 = replicate (length ies) i1
                                    (i2, e2) <- ies
                                    return $ f e1 e2
                              where ies = M.assocs mp

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data RuleResult r e = RuleContradicts r [SApplyResult (Value e)]
                    | RuleApplies     r (SApplyResult (Value e))
                    | RuleMultiple    r [SApplyResult (Value e)]
                    | RuleUnmatched   r [SApplyResult (Value e)]
                    deriving Show

showRule :: (RuleDefinition r e) => RuleResult (r e) e -> String
showRule (RuleContradicts r _) = "RuleContradicts " ++ ruleName r
showRule (RuleApplies     r _) = "RuleApplies "     ++ ruleName r
showRule (RuleMultiple    r _) = "RuleMultiple "    ++ ruleName r
showRule (RuleUnmatched   r _) = "RuleUnmatched "   ++ ruleName r

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

type ApplyRsSuccess r e = [RuleResult r e]
type ApplyRsFailure r e = (RuleResult r e, [RuleResult r e])

type ApplyRsEither r e = Either (ApplyRsSuccess r e) (ApplyRsFailure r e)

applyRules :: (RuleDefinition r e, EntryValExt e) =>
                            [r e] -> ETable e -> (ETable e, ApplyRsEither (r e) e)
applyRules rs t = res
    where res = applyRules' rs t (Left [])

applyRules' (r:rs) t (Left acc) = let (t', res) = applyARule r t
                         in if ruleContradicts res
                            then (t, Right (res, acc))
                            else applyRules' rs t' (Left (res:acc))
applyRules' [] t acc = (t, acc)

showHistoryInner :: (Show (r e), RuleDefinition r e) => ApplyRsEither (r e) e -> String

showHistoryInner (Left success)  = str
                        where str = resStr
                              resStr = do r <- success
                                          let rS = concatMap (("\n\t\t" ++) . show) (ruleResults r)
                                          " | " ++ showRule r ++ rS ++ "\n\n"
showHistoryInner (Right failure) = "!! " ++ show failure

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- Solving

data SolveInnerResult r e = NewHypotheses    (HypothesesLevel r e)
                          | FallbackRequired (RuleResult (r e) e)
                          | CanDoNothing
                          | Stopped

instance (Show (r e)) => Show (SolveInnerResult r e) where
    show (NewHypotheses hl)     = "NewHypotheses:\n"    ++ show hl
    show (FallbackRequired res) = "FallbackRequired:\n" ++ show res
    show CanDoNothing           = "CanDoNothing"
    show Stopped                = "Stopped"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

newtype Hypothesis e = Hypothesis [(Id, [Value e])]
newtype HypothesesAlt e = HypothesesAlt [Hypothesis e]


type Hypotheses r e = [(r e, HypothesesAlt e)]

data HypothesesLevel r e = HypothesesLevel {
      hypsInQueue :: Hypotheses r e
    , hypsFailed  :: Hypotheses r e
    , currentRule :: r e
    , currentHypQ :: HypothesesAlt e
    , currentHyp  :: Hypothesis e
    , currentHypF :: HypothesesAlt e
    }

showHypothesis indent (Hypothesis hs) = intercalate "\n" $ map f hs
            where f (k, vs) = indent ++ show k ++ " : " ++ show vs

showHypothesesAlt indent (HypothesesAlt hhs) =
        indent ++ "** Alternative Hypotheses:\n" ++
        intercalate betweenAlts (map (showHypothesis (indent ++ "\t")) hhs)

    where betweenAlts = "\n" ++ indent ++ replicate 10 '=' ++ "\n"


showHypotheses indent hhs = intercalate ("\n" ++ indent) hStrs
    where hStrs = concatMap (\(r, hs) -> ["| " ++ show r, hAltStr hs]) hhs
          hAltStr = showHypothesesAlt (indent ++ "\t")

instance (Show (r e)) => Show (HypothesesLevel r e) where
    show hl = "* HypothesesLevel:"      ++
              "\n  - current rule: "    ++ show (currentRule  hl) ++
              "\n  - current: "         ++ showHypothesis    "\t" (currentHyp   hl) ++
              "\n  - current queue:\n"  ++ showHypothesesAlt "\t" (currentHypQ  hl) ++
              "\n  - current tried:\n"  ++ showHypothesesAlt "\t" (currentHypF  hl) ++
              "\n  - failed hypotheses : "  ++ showHypotheses "\t\t" (hypsFailed hl) ++
              "\n  - hypotheses in queue: " ++ showHypotheses "\t\t" (hypsInQueue hl)


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class ContextTable c e where
    contextTable    :: c e -> ETable e
    updContextTable :: c e -> ETable e -> c e

class ContextHypotheses c e r where
    contextHypotheses    :: c e -> [HypothesesLevel r e]
    updContextHypotheses :: c e -> [HypothesesLevel r e] -> c e

class (ContextTable c e, ContextHypotheses c e r) => SolveContext c e r where
    solveContext :: ETable e -> [HypothesesLevel r e] -> c e


newtype ExecContext r e = ExecContext (ETable e, [HypothesesLevel r e])

newExecContext t = ExecContext (t, [])

instance ContextTable (ExecContext r) e where
    contextTable (ExecContext (t, _)) = t
    updContextTable (ExecContext (_, h)) t = ExecContext (t, h)

instance ContextHypotheses (ExecContext r) e r where
    contextHypotheses (ExecContext (_, h)) = h
    updContextHypotheses (ExecContext (t, _)) h = ExecContext (t, h)

instance SolveContext (ExecContext r) e r where
    solveContext = curry ExecContext

instance (Show (r e), Show e) => Show (ExecContext r e) where
    show (ExecContext (t, hs)) = "Table:\n" ++ show t ++ "\n\n" ++
                                 "Hypotheses:\n" ++ intercalate "\n"(map show hs) ++ "\n"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

solveProblemInner :: (SolveContext context e rule, EntryValExt e, RuleDefinition rule e) =>
    context e ->
    [rule e] ->
    Maybe Int  ->
    [ApplyRsEither (rule e) e] ->
        (context e, SolveInnerResult rule e, [ApplyRsEither (rule e) e])

solveProblemInner c rs mbMax acc = (solveContext t h, res, acc' ++ acc)
                where (t, res, acc') = solveProblemInner' stop (contextTable c) rs []
                      h = case res of NewHypotheses hl -> hl : contextHypotheses c
                                      _                -> contextHypotheses c
                      stop acc = maybe False (length acc ==) mbMax


newHypotheses rrs = NewHypotheses $
    HypothesesLevel hQueue hFailed cRule chQueue ch chFailed
    where hQueue   = tail hAlts
          (cRule, HypothesesAlt cha) = head hAlts
          ch       = head cha
          chQueue  = HypothesesAlt $ tail cha
          hFailed  = []
          chFailed = HypothesesAlt []
          hAlts = do (RuleMultiple rule rs) <- rrs
                     let hyps = map (Hypothesis . getResultEntries) rs
                     return (rule, HypothesesAlt hyps)


-- solve by depth
solveProblemInner' :: (EntryValExt e, RuleDefinition rule e) =>
    ([ApplyRsEither (rule e) e] -> Bool) ->
    ETable e ->
    [rule e] ->
    [ApplyRsEither (rule e) e] ->
    (ETable e, SolveInnerResult rule e, [ApplyRsEither (rule e) e])

-- single possibility    => RuleApplies  SImplies
-- various possibilities => RuleMultiple SPossible
solveProblemInner' stop t rs acc =
    case res of Left rrs | stop acc            -> (t, Stopped, acc)
                         | any ruleImplies rrs -> solveProblemInner' stop t' rs (res:acc)
                         | otherwise           -> tryPossible rrs
                Right (rc, rcAcc)              -> (t', FallbackRequired rc, Left rcAcc : acc)
    where (t', res) = applyRules rs t
          tryPossible rrs =
            let count = possibleCount rrs
                toApply = do (1, (rule, [SPossible rs])) <- count
                             return $ RuleApplies rule $ SImplies rs []
            in if not . null $ toApply
                then let t'' = applyRules'' t' toApply
                         res' = Left toApply
                     in solveProblemInner' stop t'' rs (res':res:acc)
               else if not . null $ count
                then let nh = newHypotheses $ map (uncurry RuleMultiple . snd)
                                                  (sortWith fst count)
                     in (t', nh, res:acc)
               else (t', CanDoNothing, res:acc)
          possibleCount rrs = do (RuleUnmatched r rs) <- rrs
                                 let prs = filter isUndetermined rs
                                 return (length prs, (r, prs))
          possibleImply = filter ((== 1) . fst)
          applyRules'' t (RuleApplies _ (SImplies r _) : rs) = applyRules'' (updSEntry t r) rs
          applyRules'' t  [] = t


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data SolveResult r e = SolveSuccess (SolveInnerResult r e)
                     | SolveFailure (SolveInnerResult r e)

instance (Show (r e)) => Show (SolveResult r e) where
    show (SolveSuccess rs) = " * ** Success ** *\n" ++ show rs
    show (SolveFailure rs) = " * ** Failure ** *\n" ++ show rs

solveResult (SolveSuccess res) = res
solveResult (SolveFailure res) = res

isSolved (SolveSuccess _) = True
isSolved _ = False

data SolveHistoryEntry rule e = SHistEntry (SolveInnerResult rule e) [ApplyRsEither (rule e) e]
                              | SHypApply  (rule e) (Hypothesis e) (HypothesesAlt e)

type SolveHistory rule e = [SolveHistoryEntry rule e]

showHistory :: (Show (r e), RuleDefinition r e) => SolveHistory r e -> String

showHistory (SHistEntry res acc : hs) =
    replicate 20 '=' ++
    "\n" ++ show res ++
    betweenHEntries  ++
    intercalate betweenHEntries (map showHistoryInner acc) ++
    "\n" ++ showHistory hs

    where betweenHEntries = "\n" ++ replicate 20 '-' ++ "\n"

showHistory (SHypApply r h ah : hs) =
    replicate 20 '=' ++
    "\n" ++ show r ++ " =>\n" ++ showHypothesis "" h ++ " :\n" ++
                                 showHypothesesAlt "\t" ah ++
    "\n" ++ showHistory hs

showHistory [] = ""


solveProblem :: (SolveContext context e rule, EntryValExt e, RuleDefinition rule e) =>
    context e -> [rule e] -> Maybe Int  -> (context e, SolveResult rule e, SolveHistory rule e)

solveProblem c rs mbMax = solveProblem' c rs mbMax []

solveProblem' c rs mbMax acc =
    let (c', res, acc') = solveProblemInner c rs mbMax []
        acc''                  = SHistEntry res acc' : acc
    in
      case res of NewHypotheses hl -> let Hypothesis hs = currentHyp hl
                                          t'' = updSEntry (contextTable c') hs
                                          c'' = updContextTable c' t''
                                          hh  = SHypApply (currentRule hl) (currentHyp hl) (currentHypQ hl)
                                      in solveProblem' c'' rs mbMax (hh : acc'')
                  _ -> (c', SolveFailure res, acc'')
--                  FallbackRequired br -> TODO



