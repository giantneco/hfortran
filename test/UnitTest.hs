module Main where

import Text.Parsec

import HFortran.Parser
import HFortran.AST

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Monad.State

main = defaultMain $ hUnitTestToTests $ tests

parseHelper :: String -> FortranTopLevel
parseHelper input =
  let v = parse program "test" input
  in case v of
    Right output -> output
    Left _ -> error "parse Error!"

tests = test [ 
  parseHelper "program main\nend program main\n"
  ~?=
  Program "main" [] []
  ]
-- runState (return 12 >>= put) 11  ~?= runState (put 12) 11
--              , runState (get >>= return) 12 ~?= runState get 12
--              , runState (get >>= (\x -> put x >>= return)) 12
--                ~?= runState (get >>= put >>= return) 12]
