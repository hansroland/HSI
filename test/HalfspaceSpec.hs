module HalfspaceSpec (spec) where

import Test.Hspec

import Data.Graph.HSI.Test

import qualified Data.Vector.Unboxed as VU

spec :: Spec
spec = do
  describe "Halfspace" $ do
    it "normalizing a vector" $ do
        let vec = VU.fromList [1,1,0,10]:: VU.Vector Double
        show (normalize (Halfspace vec)) `shouldBe`
            "Halfspace [0.7071067811865475,0.7071067811865475,0.0,10.0]"



