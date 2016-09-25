module Tennis where

data Player = PlayerOne | PlayerTwo deriving (Eq, Show)

data Point = Love | Fifteen | Thirty deriving (Eq, Show)

data PointsData = PointsData
  { playerOnePoint :: Point
  , playerTwoPoint :: Point
  } deriving (Eq, Show)

data FourtyData = FourtyData
  { player           :: Player
  , otherPlayerPoint :: Point
  } deriving (Eq, Show)

data Score =
    Points PointsData
  | Fourty FourtyData
  | Deuce
  | Advantage Player
  | Game Player
  deriving (Eq, Show)

otherPlayer :: Player -> Player
otherPlayer PlayerOne = PlayerTwo
otherPlayer PlayerTwo = PlayerOne

scoreWhenDeuce :: Player -> Score
scoreWhenDeuce winner = Advantage winner

scoreWhenAdvantage :: Player -> Player -> Score
scoreWhenAdvantage currentState winnerOfBall
  | currentState == winnerOfBall = Game winnerOfBall
  | otherwise                    = Deuce

nextPoints :: Point -> Maybe Point
nextPoints Love    = Just Fifteen
nextPoints Fifteen = Just Thirty
nextPoints Thirty  = Nothing

pointTo :: Player -> Point -> PointsData -> PointsData
pointTo PlayerOne point current = current { playerOnePoint = point }
pointTo PlayerTwo point current = current { playerTwoPoint = point }

pointFor :: Player -> PointsData -> Point
pointFor PlayerOne current = playerOnePoint current
pointFor PlayerTwo current = playerTwoPoint current

scoreWhenFourty :: FourtyData -> Player -> Score
scoreWhenFourty (FourtyData player' points) winner
  | player' == winner = Game winner
  | otherwise         = case nextPoints points of
      Just p  -> Fourty $ FourtyData player' p
      Nothing -> Deuce

  -- | points == Thirty = Deuce
  -- | otherwise        = Fourty $ FourtyData player $ nextPoints points

scoreWhenGame :: Player -> Score
scoreWhenGame winner = Game winner

scoreWhenPoints :: PointsData -> Player -> Score
scoreWhenPoints current winner = case nextPoints $ pointFor winner current of
    Just np -> Points $ pointTo winner np current
    Nothing -> Fourty FourtyData { player = winner,
                                   otherPlayerPoint = pointFor (otherPlayer winner) current
                                 }

score :: Player -> Score -> Score
score winner current = case current of
  Points p    -> scoreWhenPoints p winner
  Fourty f    -> scoreWhenFourty f winner
  Deuce       -> scoreWhenDeuce winner
  Advantage a -> scoreWhenAdvantage a winner
  Game g      -> scoreWhenGame g

newGame :: Score
newGame = Points PointsData { playerOnePoint = Love, playerTwoPoint = Love }

scoreSeq :: [Player] -> Score
scoreSeq wins = foldr score newGame wins
