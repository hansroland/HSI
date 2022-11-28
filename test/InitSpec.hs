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
    "1 => [2,3,4] Edge empty 2 [] Hidden\n" ++
    "2 => [5,6] Edge empty 1 [2] Hidden\n" ++
    "3 => [5,7] Edge empty 1 [3] Hidden\n" ++
    "4 => [7,6] Edge empty 1 [4] Hidden\n" ++
    "5 => [] Vertex empty [0.0,2.0] [2,3]\n" ++
    "6 => [] Vertex empty [-4.0,-2.0] [2,4]\n" ++
    "7 => [] Vertex empty [4.0,-2.0] [3,4]\n"
