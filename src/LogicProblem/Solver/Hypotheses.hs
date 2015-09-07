
module LogicProblem.Solver.Hypotheses (

  Hypothesis(..)
, HypothesesAlt(..)
, Hypotheses
, HypothesesLevel(..)

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

newHypotheses rrs | not . null $ hAlts =
                        HypothesesLevel hQueue hFailed cRule chQueue ch chFailed
                  | otherwise = error "cannot create newHypotheses"
    where hQueue   = tail hAlts
          (cRule, HypothesesAlt cha) = head hAlts
          ch       = if not . null $ cha then head cha
                                         else error "CCC"
          chQueue  = HypothesesAlt $ tail cha
          hFailed  = []
          chFailed = HypothesesAlt []
          hAlts = do (RuleMultiple rule rs) <- rrs
                     let hyps = map (Hypothesis . getResultEntries) rs
                     if not . null $ hyps then return (rule, HypothesesAlt hyps)
                                          else []

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
                                in if not . null $ lstHypAlt q then hl {
                                         currentHypF = HypothesesAlt []
                                       , currentHyp  = head $ lstHypAlt q
                                       , currentHypQ = mapHypAlt tail q
                                       , currentRule = r
                                       , hypsFailed  = (currentRule hl, failed) : hypsFailed hl
                                       , hypsInQueue = tail $ hypsInQueue hl
                                       } :hls
                                       else error "AAA"
                        | otherwise = nextHypotheses hls -- TODO: drop the current hyp level
    where
          failed = mapHypAlt (currentHyp hl :) (currentHypF hl)

nextHypotheses [] = [] -- error "no more hypotheses"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
