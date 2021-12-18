{-# LANGUAGE NumericUnderscores #-}
module Main where

-- import           Control.Lens
import           Control.Monad
import qualified Data.Ext as Current
import qualified Data.Ext.Church as Church
import qualified Data.Ext.Zero as Zero
-- import           Data.Function (on)
import qualified Data.List as List
import           Data.Ord (comparing)
import           System.Random
import           Test.Tasty.Bench

-- sorted n

algo                  :: Ord b => (t -> b) -> [Int] -> (Int -> Int -> t) -> [t]
algo cmp xs construct = List.sortBy (comparing cmp) . go $ xs
  where
    go = \case
      (a:b:ys) -> construct a b : go ys
      _        -> []

gen   :: Int -> IO [Int]
gen n = replicateM n randomIO

benchWith   :: Int -> Benchmark
benchWith n = env (gen n) $ \xs ->
    bgroup ("sorting " <> show n)
      [ bgroup "all"
        [ bench "current"            $ nf (algo id xs) $ (Current.:+)
        , bench "pairs"              $ nf (algo id xs) $ (,)
        , bench "Curch"              $ nf (algo id xs) $ (Church.:+)
        , bench "Zero.Ext [WithExt]" $ nf (algo id xs) $ Zero.construct @Zero.WithExt
        , bench "Zero.Ext (:+)"      $ nf (algo id xs) $ (Zero.:+)
        , bench "Zero.Ext [NoExt]"   $ nf (algo id xs) $ Zero.construct @Zero.NoExt
        ]
      , bgroup "onCore"
        [ bench "current"            $ nf (algo Current._core xs) $ (Current.:+)
        , bench "pairs"              $ nf (algo fst xs)           $ (,)
        , bench "Curch"              $ nf (algo Church._core xs)  $ (Church.:+)
        , bench "Zero.Ext [WithExt]" $ nf (algo Zero._core' xs)   $ Zero.construct @Zero.WithExt
        , bench "Zero.Ext [NoExt]"   $ nf (algo Zero._core' xs)   $ Zero.construct @Zero.NoExt
        , bench "Zero.Ext (:+)"      $ nf (algo Zero._core  xs)   $ (Zero.:+)
        , bench "None, Ints"         $ nf (algo id xs)            $ \a _ -> a
        ]
      ]


main :: IO ()
main = defaultMain
  [ benchWith 1_000_000
  ]
