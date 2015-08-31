
module LogicProblem.Solver.Hypotheses (

  Hypothesis(..)
, HypothesesAlt(..)
, Hypotheses
, HypothesesLevel(..)

--, lstHypAlt
--, mapHypAlt

, newHypotheses
, nextHypotheses

) where

import LogicProblem.Solver.Env
import LogicProblem.Solver.Def

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

lstHypAlt (HypothesesAlt ha)   = ha
mapHypAlt f (HypothesesAlt ha) = HypothesesAlt $ f ha

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

newHypotheses rrs =
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

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

nextHypotheses (hl:hls) | not . null . lstHypAlt $ currentHypQ hl =
                                let q = currentHypQ hl
                                in
                                   hl  { currentHypF = failed
                                       , currentHyp  = head . lstHypAlt $ q
                                       , currentHypQ = mapHypAlt tail q
                                       } :hls
                        | not . null $ hypsInQueue hl =
                                let (r, q) = head $ hypsInQueue hl
                                in hl { currentHypF = HypothesesAlt []
                                       , currentHyp  = head $ lstHypAlt q
                                       , currentHypQ = mapHypAlt tail q
                                       , currentRule = r
                                       , hypsFailed  = (currentRule hl, failed) : hypsFailed hl
                                       , hypsInQueue = tail $ hypsInQueue hl
                                       } :hls
                        | otherwise = nextHypotheses hls -- TODO: drop the current hyp level
    where
          failed = mapHypAlt (currentHyp hl :) (currentHypF hl)

nextHypotheses [] = [] -- error "no more hypotheses"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
