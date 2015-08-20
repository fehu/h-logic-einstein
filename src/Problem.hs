{-# LANGUAGE MultiParamTypeClasses
            , FlexibleInstances
--            , UndecidableInstances
          #-}

module Problem (
  Statement(..)
--, Value(..)
) where


-- TODO:  use `mirror`

import Data.Foldable (toList)
import Data.Map as Map (Map, keys, lookup, filter, update, member, toList, elems)
import Data.Maybe (fromMaybe, isJust)

flatMap f = concatMap (Data.Foldable.toList . f)

_TODO_ = undefined


class Table t v id a where
    vars :: t v id a -> [v]
    ids  :: t v id a -> [id]

    get   :: t v id a -> v -> id -> Maybe a
    isSet :: t v id a -> v -> id -> Bool
    getId :: t v id a -> v -> a  -> Maybe id

    set   :: t v id a -> v -> id -> a -> t v id a
    clear :: t v id a -> v -> id ->      t v id a

    listVar :: t v id a -> v  -> [(id, a)]
    listId  :: t v id a -> id -> [(v, a)]

    isSet t v id = isJust $ get t v id

--class Value v var val where
--    varOf :: v -> var
--    valOf :: v -> val

data Statement v a = State (v,a) (v,a)
                   | Constraint  v      (a -> Bool)
                   | Constraint2 v v    ((a,a) -> Bool)
                   | Cond2 v v          ((a,a) -> (a,a) -> Bool)
                   | Cond3 v v v        ((a,a,a) -> (a,a,a) -> Bool)

instance (Show v, Show a) => Show (Statement v a) where
    show (State s1 s2)         = "State " ++ show s1  ++ " " ++ show s2
    show (Constraint v _)      = "Constraint over " ++ show v
    show (Constraint2 v1 v2 _) = "Constraint over " ++ show v1 ++ " and " ++ show v2
    show (Cond2 v1 v2 _)       = "Condition for " ++ show v1 ++ " and " ++ show v2
    show (Cond3 v1 v2 v3 _)    = "Condition for " ++ show v1 ++ ", " ++ show v2 ++ " and " ++ show v3


class StatementApplication container v id a where
    applyStatement :: container v id a -> Statement v a
                    -> (StatementApplicationResult v id a, container v id a)

data StatementApplicationResult v id a = Broken (Statement v a)
                                       | Confirmed (Statement v a)
                                       | Apply [(v,a)] (Statement v a)
                                       | Possibilities [id] (Statement v a)

extractBroken (Broken st) = Just st
extractBroken _           = Nothing

extractApply (Apply a st) = Just (a, st)
extractApply _            = Nothing

data StatementsApplicationResult c v id a = Success{ containerRes :: c v id a
                                                   , history :: [StatementApplicationResult v id a]
                                                   , applied :: [Statement v a]
                                                   }
                                          | Failure (Statement v a)

applyStatements :: (StatementApplication c v id a) =>
                    c v id a -> [Statement v a] -> StatementsApplicationResult c v id a

applyStatements c sts = case flatMap extractBroken results of []   -> Success cRes results applied
                                                              [bs] -> Failure bs
                       where (results, cRes) = applyStatements' c sts []
                             applied = flatMap (fmap snd . extractApply) results

applyStatements' c (st:sts) acc = case applyStatement c st of (res@(Broken _), _) -> (res:acc, c)
                                                              (res, c')           -> (res:acc, c')
applyStatements' c [] acc = (acc, c)


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data ATable v id a = ATable (Map v (Map id a))
--ATable [(v, (id, a))]

updateATable upd (ATable t) v id =
    let mb = do vs <- Map.lookup v t
                if member id vs then let vs' = update upd id vs
                                     in return $ ATable $ update (\_ -> Just vs') v t
                                else error $ "not found: " ++ show id
    in fromMaybe (error $ "not found: " ++ show v) mb

instance (Ord v, Ord id, Eq a, Show v, Show id) => Table ATable v id a where
    vars (ATable t) = keys t
    ids (ATable t) = case Map.toList t of (_,h):_ -> keys h
                                          []      -> []
        --TODO: ensure all id lists are the same
    get   (ATable t) v id = Map.lookup v t >>= Map.lookup id
    getId (ATable t) v a  = do vs <- Map.lookup v t
                               case keys $ Map.filter (== a) vs of []   -> Nothing
                                                                   [id] -> Just id
                                                                   _    -> fail "a value used twice or more"

    set t v id a = updateATable (const $ Just a) t v id
    clear = updateATable (const Nothing)

    listVar (ATable t) v = maybe [] Map.toList (Map.lookup v t)
    listId  (ATable t) id = do (v, rs) <- Map.toList t
                               maybe [] (\a -> [(v,a)]) (Map.lookup id rs)

findPossibilities t vs = do id <- ids t
                            case flatMap (\v -> get t v id) vs of [] -> return id
                                                                  _  -> []

class HasZero a where zero :: a

findPossibilitiesC2 f t (v1,v2) = do id <- ids t
                                     let g v = fromMaybe zero $ get t v id
                                     let ps = (g v1, g v2)
                                     if f ps then return id
                                             else []


match2F2 t st v1 v2 id1 id2 p1 p2
    | all not varsSet = undefined -- TODO
    | and varsSet     = (Broken st, t)
    | otherwise       = let toApply = do (i, (a1, a2)) <- [(id1, p1), (id2, p2)]
                                         let vas = do (v, a) <- [(v1, a1), (v2, a2)]
                                                      if isSet t v i then []
                                                                     else return (v, a)
                                         if not $ null vas then return (i, vas)
                                                           else []
                        in undefined -- apply

--    case (set1, set2) of (True, False) -> apply v2 id2 a2
--                         (False, True) -> apply v1 id1 a1
--                         (True, True)  -> (Broken st, t)
--                         _             -> (Possibilities possibl st, t)
    where isset = [isSet t v1, isSet t v2]
          varsSet = flatMap (`map` [id1, id2]) isset
--          apply = applyAndSetL t st


match2C t st id v1 a1 v2 a2 =
    case (set1, set2) of (True, False) -> apply v2 id a2
                         (False, True) -> apply v1 id a1
                         (True, True)  -> (Broken st, t)
                         _             -> undefined
    where set1 = isSet t v1 id
          set2 = isSet t v2 id
          apply = applyAndSet t st

applyAndSet  t st v id a = (Apply [(v, a)] st, set t v id a)
--applyAndSetL t st idVas = (Apply (flatMap  vas) st, f t vas)
--                        where f t' ((v, a):vas') = let t'' = set t' v id a
--                                                   in f t'' vas'
--                              f t' []            = t'


--applyAndSetL t st id vas = (Apply vas st, f t vas)
--                        where f t' ((v, a):vas') = let t'' = set t' v id a
--                                                   in f t'' vas'
--                              f t' []            = t'

instance (Ord v, Ord id, Eq a, Show v, Show id, HasZero a) => StatementApplication ATable v id a where
    applyStatement t st@(State (v1,a1) (v2,a2)) = -- TODO mirror
        case getId t v1 a1 of Just id -> if isSet t v2 id then (Broken st, t)
                                                          else (Apply [(v2,a2)] st, set t v2 id a2)
                              _       -> (Possibilities (findPossibilities t [v1,v2]) st, t)

    applyStatement t st@(Constraint v f) =
        case satisfying of [(id, a)] -> if isSet t v id then (Broken st, t)
                                                        else (Apply applied st, set t v id a)
                           s         -> (Possibilities (map fst s) st, t)
        where satisfying = Prelude.filter (f . snd) (listVar t v)
              applied    = map (\s -> (v, snd s)) satisfying

    applyStatement t st@(Constraint2 v1 v2 f) =
        case satisfying of [((id, a1), (_, a2))] -> match2C t st id v1 a1 v2 a2
                                              where possibl = findPossibilitiesC2 f t (v1,v2)
--                           _                     -> (Possibilities (findPossibilities t [v1,v2]) st, t)
        where satisfying = Prelude.filter (h f) $ zip (listVar t v1) (listVar t v2)
              h g ((_, a1), (_, a2)) = g (a1,a2)

    applyStatement t st@(Cond2 v1 v2 f) =
        case satisfying of [((id1, p1), (id2, p2))] -> undefined --  match2F2 t st v1 id1 p1 v2 id2 p2
        where lst = do (id, a1) <- listVar t v1
                       (_,  a2) <- listVar t v2
                       return (id, (a1, a2))
              satisfying = do (id1, p1) <- lst
                              (id2, p2) <- lst
                              if id1 /= id2 && f p1 p2 then return ((id1, p1), (id2, p2))
                                                       else []
              possibl = undefined


        --zip (listVar t v1) (listVar t v2)
--              params id =
--        where satisfying = Prelude.filter (h f) $ zip (listVar t v1) (listVar t v2)
--              h g ((id1, a1), (id2, a2)) = g (a1,a2)


--              lstVar v = map snd (listVar t v)
--              applied    = map (\s -> (v, snd s)) satisfying
    -- TODO



