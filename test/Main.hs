module Main (main) where

import Test.Hspec
import qualified Data.HexSpaceSpec

main :: IO ()
main = hspec Data.HexSpaceSpec.spec
