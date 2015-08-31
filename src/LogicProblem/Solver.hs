
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
                toApply = do (1, (rule, [RPossible rs])) <- count
                             return $ RuleApplies rule $ RImplies rs []
            in if not . null $ toApply
                then let t'' = applyRules'' t' toApply
                         res' = Left toApply
                     in solveProblemInner' stop t'' rs (res':res:acc)
               else if not . null $ count
                then let nh = newHyp $ map (uncurry RuleMultiple . snd)
                                           (sortWith fst count)
                     in (t', nh, res:acc)
               else (t', CanDoNothing, res:acc)
          possibleCount rrs = do (RuleUnmatched r rs) <- rrs
                                 let prs = filter isUndetermined rs
                                 return (length prs, (r, prs))

newHyp = NewHypotheses . newHypotheses

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

solveProblem :: (Eq (rule e), SolveContext context e rule, EntryValExt e, RuleDefinition rule e) =>
    context e -> [rule e] -> Maybe Int  -> (context e, SolveResult rule e, SolveHistory rule e)

solveProblem c rs mbMax = solveProblem' c rs mbMax []

solveProblem' c rs mbMax acc =
    let (c', res, acc') = solveProblemInner c rs mbMax []
        acc''                  = SHistEntry res acc' : acc
    in
      case res of NewHypotheses hl -> let Hypothesis hs = currentHyp hl
                                          t'' = updREntry (contextTable c') hs
                                          c'' = updContextTable c' t''
                                          hh  = SHypApply (currentRule hl) (currentHyp hl) (currentHypQ hl)
                                      in solveProblem' c'' rs mbMax (hh : acc'')
                  FallbackRequired br -> let fb = fallback (contextTable c') (contextHypotheses c') acc
                                             (t'', h'', hist) = fb
                                             c'' = solveContext t'' h''
                                         in if not . null $ h''
                                            then solveProblem' c'' rs mbMax (hist ++ acc'')
                                            else (c'', SolveFailure CanDoNothing, acc'')
                  _ -> (c', SolveFailure res, acc'')


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
