{-# Language RecordWildCards, NamedFieldPuns #-}
module InitSpec (spec) where

import Test.Hspec

import Data.Graph.HSI.Test

spec :: Spec
spec = do
  describe "Triangle" $ do
    it "create a triangle" $ do
        let (Polytope {polyDag}) = mkTriangle
        show polyDag `shouldBe` triangle1_print

triangle1_print :: String
triangle1_print = "DAG start = 1\n" ++
    "1 => [2,3,4] Nonvert 2 []  empty\n" ++
    "2 => [5,6] Nonvert 1 [2]  empty\n" ++
    "3 => [5,7] Nonvert 1 [3]  empty\n" ++
    "4 => [7,6] Nonvert 1 [4]  empty\n" ++
    "5 => [] Vertex [0.0,2.0] [2,3]  empty\n" ++
    "6 => [] Vertex [-4.0,-2.0] [2,4]  empty\n" ++
    "7 => [] Vertex [4.0,-2.0] [3,4]  empty\n"
