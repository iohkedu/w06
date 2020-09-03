module W0601
    ( -- * the Robot DSL
      Robot (..), RobotSem (..), foldRobot
    ) where

-- | DSL for a simple robot.
data Robot =
      Rest                 -- ^ Does not consume energy; does not do anything, does not take any time.
    | Go Int               -- ^ Go the given number of steps forward, taking one energy unit per step. Goes backwards if the number of steps is negative.
    | TurnRight            -- ^ Turns 90 degrees to the right, which costs one energy unit.
    | TurnLeft             -- ^ Turns 90 degrees to the left, which costs one energy unit.
    | Seq Robot Robot      -- ^ Sequences the two given commands.
    | Repeat Int Robot     -- ^ Repeats the given command as often as indicated by the first argument. Just rests if the number of repetitions is zero or negative.
    deriving Show

-- | Data needed to define semantics of type @d@ for the 'Robot'-DSL.
data RobotSem d = RobotSem
    { rest      :: d
    , go        :: Int -> d
    , turnRight :: d
    , turnLeft  :: d
    , seq_      :: d -> d -> d
    , repeat_   :: Int -> d -> d
    }

-- | Calculates the semantics specified by the first argument.
foldRobot :: RobotSem d -> Robot -> d
foldRobot sem Rest         = rest sem
foldRobot sem (Go n)       = go sem n
foldRobot sem TurnRight    = turnRight sem
foldRobot sem TurnLeft     = turnLeft sem
foldRobot sem (Seq r1 r2)  = seq_ sem (foldRobot sem r1) (foldRobot sem r2)
foldRobot sem (Repeat n r) = repeat_ sem n (foldRobot sem r)
