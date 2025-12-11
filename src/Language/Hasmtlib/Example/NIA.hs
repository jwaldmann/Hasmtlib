{-# LANGUAGE DataKinds, TypeFamilies, LambdaCase #-}

import Language.Hasmtlib
import Prelude hiding (and,or,not,(&&),(||),any,all)
import Control.Monad (replicateM)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics
import System.Environment

newtype M e = M [[e]] deriving (Generic, Show)

matrix dim = do
  xss <- replicateM dim  $ replicateM dim $ var @IntSort
  assert $ all (all (>=? 0)) xss
  assert $ head (head xss) >=? 1 && last (last xss) >=? 1
  return $ M xss

instance Num e => Num (M e) where
  M a * M b = M $ flip map a $ \ row ->
    flip map (L.transpose b) $ \ col ->
      sum $ zipWith (*) row col

instance Codec e => Codec (M e) where
  type Decoded (M e) = M (Decoded e)
  decode s (M xss) = M <$> decode s xss
  encode (M xss) = M $ map (map encode) xss
instance Equatable e => Equatable (M e)
instance Orderable e => Orderable (M e) where
  M xss >? M yss = all2 (all2 (>=?)) xss yss
     && last (head xss) >? last (head yss)
  
all2 f xs ys = and $ zipWith f xs ys

main = getArgs >>= \ [l, r, d] -> work l r (read d)

test1 = work "ab" "baa" 5

work lhs rhs dim = do
  let sigma = S.fromList $ lhs <> rhs
  res <- solveWith @SMT (solver $ debugging noisy z3) $ do
    setLogic "QF_NIA"
    setSharingMode StableNames
    i <- fmap M.fromList
       $ traverse (\ c -> (c,) <$> matrix dim)
       $ S.toList sigma
    let value i w = foldr1 (*) $ map (i M.!) w
    assert $ value i lhs >? value i rhs
    return i
  print res
