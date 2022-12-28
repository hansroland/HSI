module DagSpec (spec) where

import Test.Hspec

import DagTriangle1
import DagTriangle2
import DagTriangle4

import Data.Graph.HSI.Test

spec :: Spec
spec = do
  describe "First simple tests" $ do
    it "Build a first DAG structure for a triangle" $ do
        show triangle11 `shouldBe` rslt_triangle11
    it "Get the grandchildren of a node" $ do
        show grandkids `shouldBe` rslt_grandkids


  describe "Storing Processing Sequence in dag" $ do
    it "Sequence postprocessing single without state" $ do
         show triangle22 `shouldBe` rslt_triangle22
    it "Sequence postprocessing multiple with state" $ do
         show triangle23 `shouldBe` rslt_triangle23

  describe "filter functions in postorder" $ do
    it "postOrderSingleNodeFilter" $ do
          show triangle42 `shouldBe` rslt_triangle42


rslt_sequence1 :: String
rslt_sequence1 = "[1,2,5,6,3,7,4]"

rslt_grandkids :: String
rslt_grandkids = "[5,6,7]"


