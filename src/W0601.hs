module W0601
    ( -- * the Robot DSL
      Robot (..), RobotSem (..), foldRobot, energySem
      -- * Subtask W6.1.1
    , stepsSem
      -- * Subtask W6.1.2
    , squareDistance
      -- * Subtask W6.1.3
    , SimpleRobot (..), simpleSem
      -- * Subtask W6.1.4
    , SimpleRobotSem (..), foldSimpleRobot, simpleEnergySem
      -- * Subtask W6.1.5
    , prop_energy_simpleEnergy
    ) where

import Numeric.Natural (Natural)
import Test.QuickCheck

-- | DSL for a simple robot.
data Robot =
      Rest                 -- ^ Does not consume energy; does not do anything, does not take any time.
    | Go Int               -- ^ Goes the given number of steps forwards, taking one energy unit per step. Goes backwards if the number of steps is negative.
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

-- | Defines semantics of energy consumption for the 'Robot'-DSL.
--
-- >>> foldRobot energySem $ Repeat 4 $ Go 5 `Seq` TurnRight
-- 24
--
energySem :: RobotSem Int
energySem = RobotSem
    { rest      = 0
    , go        = abs
    , turnRight = 1
    , turnLeft  = 1
    , seq_      = (+)
    , repeat_   = \n x -> (max 0 n) * x
    }

-- Subtask W6.1.1

-- | Semantics for calculating the total number of steps (forwards and backwards) that
-- the robot will go (turning does not count, only going).
--
-- >>> foldRobot stepsSem $ Go 222 `Seq `Go (-333) `Seq` Repeat 1000 TurnRight
-- 555
--
stepsSem :: RobotSem Int
stepsSem = error "TODO: implement stepsSem"

-- Subtask W6.1.2

-- | Calculates the /square/ of the distance (measured in steps)
-- from the starting point to the end point for the given command,
--
-- >>> squareDistance $ Go 1000 `Seq` Go (-1000)
-- 0
--
-- >>> squareDistance $ Go 3 `Seq` Repeat 101 TurnLeft `Seq` Go 4
-- 25
--
-- >>> squareDistance $ Repeat 444 $ Go 101 `Seq` TurnRight
-- 0
--
squareDistance :: Robot -> Natural
squareDistance = error "TODO: implement distance"

-- Subtask W6.1.3

-- | A simpler DSL for the same robot.
data SimpleRobot =
      SimpleRest                  -- ^ Does not consume energy; does not do anything, does not take any time.
    | SimpleGo Int SimpleRobot    -- ^ Goes the given number of steps forwards (or backwards if the number is negative), then continues with the second argument.
    | SimpleTurnRight SimpleRobot -- ^ Turns 90 degrees to the right, then continues with the second argument.
    | SimpleTurnLeft SimpleRobot  -- ^ Turns 90 degrees to the left, then continues with the second argument.
    deriving Show

-- | Semantics for faithfully translating the 'Robot'-DSL into the simplified 'SimpleRobot'-DSL.
--
-- >>> foldRobot simpleSem $ (Go 1 `Seq` Go 2) `Seq` Go 3
-- SimpleGo 1 (SimpleGo 2 (SimpleGo 3))
--
-- >>> foldRobot simpleSem $ Repeat 4 $ Go 2 `Seq` TurnLeft
-- SimpleGo 2 (SimpleTurnLeft (SimpleGo 2 (SimpleTurnLeft (SimpleGo 2 (SimpleTurnLeft (SimpleGo 2 (SimpleTurnLeft SimpleRest)))))))
--
simpleSem :: RobotSem SimpleRobot
simpleSem = error "TODO: implement simpleSem"

-- Subtask W6.1.4

-- Data needed to define semantics of type @d@ for the 'SimpleRobot'-DSL.
data SimpleRobotSem d = SimpleRobotSem
    { -- TODO: fill in the required fields.
    }

-- | Calculates the semantics specified by the first argument.
foldSimpleRobot :: SimpleRobotSem d -> SimpleRobot -> d
foldSimpleRobot = error "TODO: implement foldSimpleRobot"

-- | Defines semantics of energy consumption for the 'SimpleRobot'-DSL.
--
-- >>> foldSimpleRobot simpleEnergySem $ SimpleGo 5 $ SimpleTurnRight $ SimpleGo (-3) SimpleRest
-- 9
--
simpleEnergySem :: SimpleRobotSem Int
simpleEnergySem = error "TODO: implement simpleEnergySem"

-- Subtask W6.1.5

instance Arbitrary Robot where
    arbitrary = error "TODO: implement arbitrary"
    shrink = error "TODO: implement shrink"

-- | States that energy consumption of a command in the 'Robot'-DSL as specified by 'energySem'
-- is the same as energy consumption of the translated command in the 'SimpleRobot'-DSL as specified by 'simpleEnergySem'.
--
-- prop> prop_enery_simpleEnery
--
prop_energy_simpleEnergy :: Robot -> Property
prop_energy_simpleEnergy = error "TODO: implement prop_energy_simpleEnergy"
