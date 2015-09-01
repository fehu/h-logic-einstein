
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
--                       | not (null canImply) && null imply      = (FallbackRequired imply, res)
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

--solveProblem' :: (Eq (rule e), SolveContext context e rule, EntryValExt e, RuleDefinition rule e) =>
--    context e ->
--    [rule e] ->
--    Maybe Int ->
--    SolveHistory rule e ->
--    (context e, SolveResult rule e, SolveHistory rule e)

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

--solveProblemInner :: (SolveContext context e rule, EntryValExt e, RuleDefinition rule e) =>
--    context e ->
--    [rule e] ->
--    Maybe Int  ->
--    [[RuleResult (rule e) e]] ->
--        (context e, SolveInnerResult rule e, [[RuleResult  (rule e) e]])
--
--solveProblemInner c rs mbMax acc = (solveContext t h, res, acc' ++ acc)
--                where (t, res, acc') = solveProblemInner' stop (contextTable c) rs []
--                      h = case res of NewHypotheses hl -> hl : contextHypotheses c
--                                      _                -> contextHypotheses c
--                      stop acc = maybe False (length acc ==) mbMax
--
---- solve by depth
--solveProblemInner' :: (EntryValExt e, RuleDefinition rule e) =>
--    ([[RuleResult (rule e) e]] -> Bool) ->
--    ETable e ->
--    [rule e] ->
--    [[RuleResult (rule e) e]] ->
--    (SolveInnerResult rule e, [[RuleResult (rule e) e]])
--
---- single possibility    => RuleApplies  SImplies
---- various possibilities => RuleMultiple SPossible
--solveProblemInner' stop t rs acc
--                         | stop acc                   = (Stopped, acc)
--                         | any ruleImplies rrs        = solveProblemInner' stop t rs (rrs:acc)
--                         | not . null $ contradicting = (FallbackRequired contradicting, rrs : acc)
--                         | otherwise                  = tryPossible rrs
--    where rrs = executeRules rs t
--          possibleCount = do (RuleUnmatched r rs) <- rrs
--                             let prs = filter isUndetermined rs
--                             return (length prs, (r, prs))
--          toApply = do (1, (rule, [RPossible rs])) <- possibleCount rrs
--                       return $ RuleApplies rule $ RImplies rs []
--          contradicting = filter ruleContradicts rrs
--          thePossible = map (uncurry RuleMultiple . snd)
--                            (sortWith fst $ possibleCount rrs)
----          t'' = applyRules'' t' . toApply
--          tryPossible rrs | not . null $ toApply       =
--          --(applyRules'' t' $ toApply rrs, , res:acc)
--                          | not . null $ possibleCount = (newHyp rrs,   rrs:acc)
--                          | otherwise                  = (CanDoNothing, rrs:acc)
--
--
---- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
--
--solveProblem :: (Eq (rule e), SolveContext context e rule, EntryValExt e, RuleDefinition rule e) =>
--    context e -> [rule e] -> Maybe Int  -> (context e, SolveResult rule e, SolveHistory rule e)
--
--solveProblem c rs mbMax = solveProblem' c rs mbMax []
--
--solveProblem' c rs mbMax acc =
--    let (c', res, acc') = solveProblemInner c rs mbMax []
--        acc''                  = SHistEntry res acc' : acc
--    in
--      case res of NewHypotheses hl -> let Hypothesis hs = currentHyp hl
--                                          t'' = updREntry (contextTable c') hs
--                                          c'' = updContextTable c' t''
--                                          hh  = SHypApply (currentRule hl) (currentHyp hl) (currentHypQ hl)
--                                      in solveProblem' c'' rs mbMax (hh : acc'')
--                  FallbackRequired br -> let fb = fallback (contextTable c') (contextHypotheses c') acc
--                                             (t'', h'', hist) = fb
--                                             c'' = solveContext t'' h''
--                                         in if not . null $ h''
--                                            then solveProblem' c'' rs mbMax (hist ++ acc'')
--                                            else (c'', SolveFailure CanDoNothing, acc'')
--                  _ -> (c', SolveFailure res, acc'')
--

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
