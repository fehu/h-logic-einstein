{-# LANGUAGE FlexibleContexts #-}

module LogicProblem.Solver.Show (

  showRuleResult
, showHistory


) where

import Data.List (intercalate)

import LogicProblem.Rule
import LogicProblem.Solver.Def
import LogicProblem.Solver.SDef
import LogicProblem.Solver.Apply
import LogicProblem.Solver.Hypotheses
import LogicProblem.Solver.Context
import LogicProblem.Solver.History

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

showRuleResult :: (RuleDefinition r e) => RuleResult (r e) e -> String
showRuleResult (RuleContradicts r _) = "RuleContradicts " ++ ruleName r
showRuleResult (RuleApplies     r _) = "RuleApplies "     ++ ruleName r
showRuleResult (RuleMultiple    r _) = "RuleMultiple "    ++ ruleName r
showRuleResult (RuleUnmatched   r _) = "RuleUnmatched "   ++ ruleName r

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

showHistoryInner :: (Show (r e), RuleDefinition r e) => [RuleResult (r e) e] -> String

showHistoryInner res = do r <- res
                          let rS = concatMap (("\n\t\t" ++) . show) (ruleResults r)
                          " | " ++ showRuleResult r ++ rS ++ "\n\n"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

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

instance (Show (r e), RuleDefinition r e) => Show (SolveInnerResult r e) where
    show (NewHypotheses hl)     = "NewHypotheses:\n"    ++ show hl
    show (RulesApplied res)     = "RulesApplied:\n"     ++ showHistoryInner res
    show (FallbackRequired res) = "FallbackRequired:\n" ++ show res
    show CanDoNothing           = "CanDoNothing"
    show Stopped                = "Stopped"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

instance (Show (r e), Show e) => Show (ExecContext r e) where
    show (ExecContext (t, hs)) = "Hypotheses:\n" ++ intercalate "\n" (map show hs) ++ "\n"
                              ++ "Table:\n" ++ show t ++ "\n\n"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


showHistory :: (Show (r e), RuleDefinition r e) => SolveHistory r e -> String

showHistory (SHistEntry res acc : hs) =
    replicate 20 '=' ++
    "\n" ++ show res ++
    betweenHEntries  ++
    intercalate betweenHEntries (map showHistoryInner [acc]) ++
    "\n" ++ showHistory hs

    where betweenHEntries = "\n" ++ replicate 20 '-' ++ "\n"

showHistory (SHypApply r h ah : hs) =
    replicate 20 '=' ++ "apply hypothesis" ++
    "\n" ++ show r ++ " =>\n" ++ showHypothesis "" h ++ " :\n" ++
                                 showHypothesesAlt "\t" ah ++
    "\n" ++ showHistory hs

showHistory (SUnapply r h uns : hs) =
    replicate 20 '=' ++
    "\n Unapllied " ++ showHypothesis "" h ++ " for rule " ++ show r ++ ":\n\t" ++
    intercalate "\n\t" (map show uns) ++
    "\n" ++ showHistory hs

showHistory [] = ""

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

instance (Show (r e), RuleDefinition r e) => Show (SolveResult r e) where
    show (SolveFinish  rs) = " * ** Finished ** *\n" ++ show rs
    show (SolveFailure rs) = " * ** Failure ** *\n" ++ show rs

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

