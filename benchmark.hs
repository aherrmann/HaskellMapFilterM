import Criterion.Main

import Control.Monad
import Control.Monad.State
import Data.Maybe (fromJust, isJust)
import qualified Data.Map as M
import qualified Data.Traversable as T
import qualified MapWithFilterM as M'

filterMapWithState :: (Monad m, Ord k) => (v -> v -> Bool) -> M.Map k v -> StateT v m (M.Map k v)
filterMapWithState f m = do
    s <- get
    return $ M.filter (f s) m

filterMapMToAscList :: (Monad m, Ord k) => (v -> m Bool) -> M.Map k v -> m (M.Map k v)
filterMapMToAscList f m = liftM M.fromAscList $ filterM (f.snd) $ M.toAscList m

filterMapMToList :: (Monad m, Ord k) => (v -> m Bool) -> M.Map k v -> m (M.Map k v)
filterMapMToList f m = liftM M.fromList $ filterM (f.snd) $ M.toList m

filterMapMMaybe :: (Monad m, Ord k) => (v -> m Bool) -> M.Map k v -> m (M.Map k v)
filterMapMMaybe f m = (M.map (fromJust) . M.filter isJust) `liftM` T.mapM toMaybe m
  where
    toMaybe v = do
        keep <- f v
        if keep
        then return $ Just v
        else return Nothing

filterMapMFoldr :: (Monad m, Ord k) => (v -> m Bool) -> M.Map k v -> m (M.Map k v)
filterMapMFoldr f m = M.foldrWithKey constr (return M.empty) m
  where
    constr k v out = do
        keep <- f v
        if keep
        then M.insert k v `liftM` out
        else out

filterMapMFoldl :: (Monad m, Ord k) => (v -> m Bool) -> M.Map k v -> m (M.Map k v)
filterMapMFoldl f m = M.foldlWithKey constr (return M.empty) m
  where
    constr out k v = do
        keep <- f v
        if keep
        then M.insert k v `liftM` out
        else out

filterMap'M :: (Monad m, Ord k) => (v -> m Bool) -> M'.Map k v -> m (M'.Map k v)
filterMap'M f m = M'.filterM f m

filterMap'MPure :: (Monad m, Ord k) => (v -> v -> Bool) -> M'.Map k v -> StateT v m (M'.Map k v)
filterMap'MPure f m = do
    s <- get
    return $ M'.filter (f s) m

predicate :: Int -> Int -> Bool
predicate s n = n `mod` s == 0

predicateM :: Int -> State Int Bool
predicateM n = do
    s <- get
    return $ predicate s n

benchFilter = benchFilt $ filterMapWithState predicate
benchToAscList = benchFilt $ filterMapMToAscList predicateM
benchToList = benchFilt $ filterMapMToList predicateM
benchFoldr = benchFilt $ filterMapMFoldr predicateM
benchFoldl = benchFilt $ filterMapMFoldl predicateM
benchMaybe = benchFilt $ filterMapMMaybe predicateM
benchM'Filter = benchFilt' $ filterMap'MPure predicate
benchM'FilterM = benchFilt' $ filterMap'M predicateM

benchFilt filt n = (`evalState` 4) $ do
    let inp = M.fromList $ take n $ zip ['a'..] [1..]
    filt inp

benchFilt' filt n = (`evalState` 4) $ do
    let inp = M'.fromList $ take n $ zip ['a'..] [1..]
    filt inp

main = defaultMain [ mkBench "M.filter" benchFilter
                   , mkBench "toAscList" benchToAscList
                   , mkBench "toList" benchToList
                   , mkBench "foldr" benchFoldr
                   , mkBench "foldl" benchFoldl
                   , mkBench "maybe" benchMaybe
                   , mkBench "M'.filter" benchM'Filter
                   , mkBench "M'.filterM" benchM'FilterM
                   ]
  where
    mkBench name fun = bgroup name $
        map (\n -> bench (show n) $ nf fun n) [100,1000,10000,100000]
