{-# LANGUAGE MultiParamTypeClasses #-}


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

) where

import qualified Data.Map as M

import Data.Maybe    (fromMaybe, maybeToList)
import Data.List     (intercalate)
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

executeRule r t = apply
    where res = applyS (extractRule r) t
          broken = filter isFailure res
          imply  = filter isSuccess res
          apply | not . null $ broken = RuleContradicts r broken
                | length imply == 1   = RuleApplies     r (head imply)
                | not . null $ imply  = RuleMultiple    r imply
                | otherwise           = RuleUnmatched   r res

ruleContradicts (RuleContradicts _ _) = True
ruleContradicts _ = False

ruleResults (RuleContradicts _ rs) = rs
ruleResults (RuleApplies _ rs)     = [rs]
ruleResults (RuleMultiple _ rs)    = rs
ruleResults (RuleUnmatched _ rs)   = rs

--applyARule :: (RuleDefinition rule e) => rule e -> ETable e -> (ETable e, RuleResult (rule e) e)
applyARule r t =
    case executeRule r t of res@(RuleApplies _ (SImplies x _)) -> (updSEntry t x, res)
                            res                                -> (t, res)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

type ApplyRsSuccess r e = [(r e, RuleResult (r e) (Value e))]
type ApplyRsFailure r e = RuleResult (r e) (Value e)

type ApplyRsEither r e = Either (ApplyRsSuccess r e) (ApplyRsFailure r e)

applyRules :: (RuleDefinition r e, EntryValExt e) =>
                            [r e] -> ETable e -> (ETable e, ApplyRsEither r e)
applyRules rs t = res
    where res = applyRules' rs t (Left [])

applyRules' (r:rs) t (Left acc) = let (t', res) = applyARule r t
                         in if ruleContradicts res
                            then (t, Right res)
                            else applyRules' rs t' (Left ((r, res):acc))
applyRules' [] t acc = (t, acc)

showHistory :: (Show (r e)) => ApplyRsEither r e -> String

showHistory (Left success)  = str
                        where str = resStr
                              resStr = do (k, r) <- success
                                          let rS = concatMap (("\n\t\t" ++) . show) (ruleResults r)
                                          "||" ++ show k ++ rS ++ "\n\n"
--                              x = concatMap (("\t\t\n" ++) , show) success
showHistory (Right failure) = "!| " ++ show failure


--showHistory((rule, Right results):hs) = str ++ showHistory hs
--        where str    = "|| " ++ show rule ++ "\n" ++ resStr ++ "\n\n"
--              resStr = concatMap (("\n\t\t" ++) . show) results
--
--showHistory((rule, Left failure):hs) = "!| " ++ show failure

--showHistory((rule, results):hs) = str ++ showHistory hs
--        where str    = "|| " ++ show rule ++ "\n" ++ resStr ++ "\n\n"
--              resStr = concatMap (("\n\t\t" ++) . show) results

--showHistory [] = ""


