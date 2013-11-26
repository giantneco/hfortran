module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Monad.State


main = defaultMain $ hUnitTestToTests $ tests

tests = test [ runState (return 12 >>= put) 11  ~?= runState (put 12) 11
             , runState (get >>= return) 12 ~?= runState get 12
             , runState (get >>= (\x -> put x >>= return)) 12
               ~?= runState (get >>= put >>= return) 12]
