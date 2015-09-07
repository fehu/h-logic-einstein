
module LogicProblem.Solver.Unapply (

  unapply
, fallback

) where

import Data.Either(lefts)
import Data.Maybe (fromMaybe)

import LogicProblem.Solver.Def
import LogicProblem.Solver.Env
import LogicProblem.Solver.History
import LogicProblem.Solver.Hypotheses
import LogicProblem.Solver.Apply

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

unapply t rule (SHypApply r (Hypothesis vs) _ : hs) acc
    | r == rule = let si = RImplies vs []
                  in (fst $ unapply' t [RuleApplies r si] [], (r, si) : acc)
    | otherwise = error "not same rule"

unapply t rule (SHistEntry _ rs : hs) acc =
    let (t', acc') = unapply' t rs []
    in unapply t' rule hs (acc' ++ acc)

unapply t _ [] acc = (t, acc)


unapply' t (RuleApplies r i@(RImplies ws _) : rs) iacc =
    unapply' (f t ws) rs ((r,i):iacc)
    where
          f t ((i, vs):ws) = let e  = getEntry i t
                                 e' = g e vs
                                 t' = setEntry e' t
                             in f t' ws
          f t [] = t
          g = foldl (\e' v -> fromMaybe e' (clearValue v e'))

unapply' t (_:rs) iacc = unapply' t rs iacc
unapply' t [] iacc = (t, iacc)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

fallback t hls'@(hl:_) hist = (t'', nextHyps, [apHist, unHist])
    where lastHyp = currentHyp hl
          rule    = currentRule hl
          (t', unapplied) = unapply t rule hist []
          unHist  = SUnapply rule lastHyp unapplied

          nextHyps = nextHypotheses hls'
          nextHyp  = if not . null $ nextHyps then head nextHyps
                                              else error "BBB"
          ch@(Hypothesis vs) = currentHyp nextHyp
          cr      = currentRule nextHyp
          toApply = RuleApplies cr (RImplies vs [])
          t''     = applyRules'' t' [toApply]
          apHist  = SHypApply cr ch (currentHypQ nextHyp)

fallback t [] hist = (t, [], [])

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --




