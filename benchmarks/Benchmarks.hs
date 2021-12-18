module Main where

import           Control.Lens
import qualified Data.Ext as Current
import qualified Data.Ext.Zero as Zero
import qualified Data.List as List
import           Test.Tasty.Bench


-- sorted n




main :: IO ()
main = defaultMain
 []
  -- [ bgroup "sorting, units"
  --   [ bench "current"      $ nf fibo  5
  --   , bench "zero-ext"     $ nf fibo 10
  --   , bench "curch"        $ nf fibo 20
  --   ]
  -- ]

-- withN n = env
