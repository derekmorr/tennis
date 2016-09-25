import           Tennis
import           Test.Hspec
import           Test.QuickCheck

genPlayer :: Gen Player
genPlayer = elements [PlayerOne, PlayerTwo]

-- instance Arbitrary Player where
--   arbitrary = genPlayer

genPoint :: Gen Point
genPoint = elements [Love, Fifteen, Thirty]

-- instance Arbitrary Point where
--   arbitrary = elements [Love, Fifteen, Thirty]

genPointsData :: Gen PointsData
genPointsData = do
  p1 <- genPoint
  p2 <- genPoint
  return $ PointsData p1 p2

--instance Arbitrary PointsData where
--  arbitrary = genPointsData

genFourtyData :: Gen FourtyData
genFourtyData = do
  player' <- genPlayer
  point   <- genPoint
  return $ FourtyData player' point

-- instance Arbitrary FourtyData where
--   arbitrary = genFourtyData


--instance Arbitrary Game where
--  arbitrary = do
--    player <- arbitrary Player
--    return Game player

prop_deuce_wins_correct_score :: Property
prop_deuce_wins_correct_score =
  forAll genPlayer (\winner ->
    Advantage winner == scoreWhenDeuce winner)

prop_advantage_wins_correct_score :: Property
prop_advantage_wins_correct_score =
  forAll genPlayer (\winner ->
    Game winner == scoreWhenAdvantage winner winner)

prop_advantage_other_wins_correct_score :: Property
prop_advantage_other_wins_correct_score =
  forAll genPlayer (\winner ->
    Deuce == scoreWhenAdvantage winner (otherPlayer winner))

prop_fourty_correct_score :: Property
prop_fourty_correct_score =
  forAll genFourtyData (\current ->
    let p = player current
    in Game p == scoreWhenFourty current p)

prop_fourty_other_wins_correct_score :: Property
prop_fourty_other_wins_correct_score =
  forAll genFourtyData (\current ->
    let newCurrent     = current { otherPlayerPoint = Thirty }
        theOtherPlayer = otherPlayer $ player newCurrent
    in Deuce == scoreWhenFourty newCurrent theOtherPlayer)

main :: IO ()
main = hspec $
  describe "Tennis scoring rules" $ do
    it "given deuce when player wins the score is correct" $
      prop_deuce_wins_correct_score
    it "given advantage when player wins the score is correct" $
      prop_advantage_wins_correct_score
    it "given advantage when other player wins then score is correct" $
      prop_advantage_other_wins_correct_score
    it "given player: 40 when player wins the score is correct" $
      prop_fourty_correct_score
    it "given player: 40 - other:30 when other wins the score is correct" $
      prop_fourty_other_wins_correct_score
