module LogicProblem.Solver (

  solveProblem

) where

import GHC.Exts

import LogicProblem.Rule
import LogicProblem.Solver.Env
import LogicProblem.Solver.Def
import LogicProblem.Solver.SDef
import LogicProblem.Solver.Apply
import LogicProblem.Solver.Unapply
import LogicProblem.Solver.Hypotheses
import LogicProblem.Solver.Context
import LogicProblem.Solver.History

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

solveProblemInner :: (SolveContext context e rule, RuleDefinition rule e) =>
    context e ->
    [rule e] ->
    (SolveInnerResult rule e, [RuleResult (rule e) e])

-- single possibility    => RuleApplies  SImplies
-- various possibilities => RuleMultiple SPossible

solveProblemInner c rs | not $ null contradict                  = (FallbackRequired contradict     , res)
                       | not (null canImply) && null imply      = (FallbackRequired imply, res)
                       | not (null imply) || not (null toApply) = (RulesApplied  $ imply ++ toApply, res)
                       | not (null thePossible)                 = (NewHypotheses $ newHypotheses thePossible, res)
                       | otherwise                              = (CanDoNothing, res)
    where res = executeRules rs t
          t   = contextTable c

          canImply   = filter ruleImplies res
          imply      = filter notPresent  res
          contradict = filter ruleContradicts res

          notPresent (RuleApplies _ (RImplies xs _)) = null . fst  $ partitionAlreadyPresent t xs
          notPresent _                               = False

          possibleCount = do (RuleUnmatched r rs) <- res
                             let prs = filter isUndetermined rs
                             return (length prs, (r, prs))
          toApply = do (1, (rule, [RPossible rs])) <- possibleCount
                       filter notPresent $ return $ RuleApplies rule $ RImplies rs []
          thePossible = map (uncurry RuleMultiple . snd)
                            (sortWith fst (filter ((/= 1) . fst) possibleCount))

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

solveProblem :: (Eq (rule e), SolveContext context e rule, EntryValExt e, RuleDefinition rule e) =>
    context e -> [rule e] -> Maybe Int  -> (context e, SolveResult rule e, SolveHistory rule e)

solveProblem c rs mbStop = solveProblem' c rs mbStop []

solveProblem' c rs mbStop acc =
    case res of _ | stop acc            -> (c, SolveFailure Stopped, acc)
                FallbackRequired reason -> let (t', hyps', hist) = fallback t hyps acc
                                           in if null hyps'
                                              then (c, SolveFailure res, resHE : acc)
                                              else solveProblem' (solveContext t' hyps') rs mbStop (hist ++ acc)
                NewHypotheses hl        -> let Hypothesis hs = currentHyp hl
                                               t'  = updREntry t hs
                                               c'  = solveContext t' (hl : contextHypotheses c)
                                               hh  = SHypApply (currentRule hl) (currentHyp hl) (currentHypQ hl)
                                           in solveProblem' c' rs mbStop (hh : acc)
                RulesApplied rrs        -> let t' = applyRules'' t rrs
                                           in solveProblem' (updContextTable c t') rs mbStop (resHE : acc)
                _                       -> (c, SolveFailure res, resHE : acc)
    where stop acc = maybe False (length acc ==) mbStop
          (res, resH)  = solveProblemInner c rs
          resHE = SHistEntry res resH
          t    = contextTable c
          hyps = contextHypotheses c

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

