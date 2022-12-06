module HSISpec (spec) where

import Test.Hspec
import Data.Graph.HSI.Test

spec :: Spec
spec = do
  describe "HSI: Relative Hs Position 2-dim" $ do
    it "1 reuse halfspace of polytope 2-dim" $ do
        showRelPosPoly hs0 `shouldBe` "relPosP0"
    it "2 real intersection" $ do
        showRelPosPoly hs1 `shouldBe` "relPosMP"
  describe "Removal of outside nodes" $ do
    it "10 triangle 2-dim" $ do
        showInsideHsDag2dim hs1 `shouldBe` inside_nodes_10
    it "30 pyramid 3-dim" $ do
        showInsideHsDag3dim hs3 `shouldBe` inside_nodes_30
  describe "HSI do a single step 3-dim" $ do
    it "50 calculate a smaller pyramid 3-dim" $ do
        showDagAfterHsiStep hs3 `shouldBe`  dag_after_step_50


hs0 :: Halfspace
hs0 = normalize $ hsFromList  [0, 1, -2]

hs1 :: Halfspace             -- horizonal cut in the middle
hs1 = normalize $ hsFromList  [0, 1, 0]

hs3 :: Halfspace
hs3 =  hsFromList  [0,0,1, -10]


-- Show the relative position of the whole polytope
-- The polytope is the triangle
showRelPosPoly :: Halfspace -> String
showRelPosPoly hs = show $ polyRelPos $ hsiRelPosPoly hs mkTriangle

-- Show the Dag with only the remaining inside nodes
-- The polytope is the triangle
showInsideHsDag2dim :: Halfspace -> String
showInsideHsDag2dim hs = show $ polyDag $ hsiIntersectHMin $ hsiRelPosPoly hs mkTriangle

-- Show the Dag with only the remaining inside nodes
-- The polytope is the pyramide
showInsideHsDag3dim :: Halfspace -> String
showInsideHsDag3dim hs = show $ polyDag $ hsiIntersectHMin $ hsiRelPosPoly hs mkPyramid

showDagAfterHsiStep :: Halfspace -> String
showDagAfterHsiStep hs = show $ polyDag <$> hsiStep mkPyramid hs

inside_nodes_10 :: String
inside_nodes_10 = "DAG start = 1\n" ++
    "1 => [2,3] Nonvert relPosMP 2 [] Hidden\n" ++
    "2 => [5] Nonvert relPosMP 1 [2] Hidden\n" ++
    "3 => [5] Nonvert relPosMP 1 [3] Hidden\n" ++
    "5 => [] Vertex relPosP [0.0,2.0] [2,3]\n"

inside_nodes_30 :: String
inside_nodes_30 = "DAG start = 1\n" ++
    "1 => [2,3,5,6] Nonvert relPosMP 3 [] Hidden\n" ++
    "2 => [7,8] Nonvert relPosMP 2 [2] Hidden\n" ++
    "3 => [7,13] Nonvert relPosMP 2 [3] Hidden\n" ++
    "5 => [8,14] Nonvert relPosMP 2 [5] Hidden\n" ++
    "6 => [13,14] Nonvert relPosMP 2 [4] Hidden\n" ++
    "7 => [17] Nonvert relPosMP 1 [2,3] Hidden\n" ++
    "8 => [17] Nonvert relPosMP 1 [2,5] Hidden\n" ++
    "13 => [17] Nonvert relPosMP 1 [3,4] Hidden\n" ++
    "14 => [17] Nonvert relPosMP 1 [5,4] Hidden\n" ++
    "17 => [] Vertex relPosP [0.0,0.0,200.0] [2,3,4,5]\n"

dag_after_step_50 :: String
dag_after_step_50 = "Right DAG start = 1\n" ++
    "1 => [26,2,3,5,6] Nonvert relPosMP 3 [] Hidden\n" ++
    "2 => [20,7,8] Nonvert relPosMP 2 [2] Hidden\n" ++
    "3 => [22,7,13] Nonvert relPosMP 2 [3] Hidden\n" ++
    "5 => [24,8,14] Nonvert relPosMP 2 [5] Hidden\n" ++
    "6 => [25,13,14] Nonvert relPosMP 2 [4] Hidden\n" ++
    "7 => [18,17] Nonvert relPosMP 1 [2,3] Hidden\n" ++
    "8 => [19,17] Nonvert relPosMP 1 [2,5] Hidden\n" ++
    "13 => [21,17] Nonvert relPosMP 1 [3,4] Hidden\n" ++
    "14 => [23,17] Nonvert relPosMP 1 [5,4] Hidden\n" ++
    "17 => [] Vertex relPosP [0.0,0.0,200.0] [2,3,4,5]\n" ++
    "18 => [] Vertex relPos0 [210.0,210.0,-10.0] [2,3,6]\n" ++
    "19 => [] Vertex relPos0 [-210.0,210.0,-10.0] [2,5,6]\n" ++
    "20 => [18,19] Nonvert relPos0 1 [6,2] Hidden\n" ++
    "21 => [] Vertex relPos0 [210.0,-210.0,-10.0] [3,4,6]\n" ++
    "22 => [18,21] Nonvert relPos0 1 [6,3] Hidden\n" ++
    "23 => [] Vertex relPos0 [-210.0,-210.0,-10.0] [5,4,6]\n" ++
    "24 => [19,23] Nonvert relPos0 1 [6,5] Hidden\n" ++
    "25 => [21,23] Nonvert relPos0 1 [6,4] Hidden\n" ++
    "26 => [20,22,24,25] Nonvert relPos0 2 [6] Hidden\n"
