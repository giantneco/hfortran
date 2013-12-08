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
  parseHelper
  "program main\nend program main\n"
  ~?=
  Program "main" [] []
  ,
  parseHelper
  "program main\n end program main \n"
  ~?=
  Program "main" [] []
  ,
  parseHelper
  "program main\n integer a \nend program main\n"
  ~?=
  Program "main" [TypeDeclaration FInteger "a"] []
  ,
  parseHelper
  "program main\n integer a \nend program main\n"
  ~?=
  Program "main" [TypeDeclaration FInteger "a"] []
  ,
  parseHelper
  "program main\n integer a \nend program main\n"
  ~?=
  Program "main" [TypeDeclaration FInteger "a"] []
  ]
