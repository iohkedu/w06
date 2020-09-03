# Weekly Assignments 6

### To be submitted: Friday, 11 September 2020, 12:30 MNG

Note that some tasks may deliberately ask you to look at concepts or libraries
that we have not yet discussed in detail. But if you are in doubt about the
scope of a task, by all means ask.

Please try to write high-quality code at all times!
This means in particular that you should add comments to all parts
that are not immediately obvious. Please also pay attention to
stylistic issues. The goal is always to submit code that does not
just correctly do what was asked for, but also could be committed
without further changes to an imaginary company codebase.

## W6.1 The Robot DSL

Recall the `Robot`-DSL from our example session:

```
data Robot =
      Rest
    | Go Int
    | TurnRight
    | TurnLeft
    | Seq Robot Robot
    | Repeat Int Robot

data RobotSem d = RobotSem
    { rest      :: d
    , go        :: Int -> d
    , turnRight :: d
    , turnLeft  :: d
    , seq_      :: d -> d -> d
    , repeat_   :: Int -> d -> d
    }

foldRobot :: RobotSem d -> Robot -> d
foldRobot sem Rest         = rest sem
foldRobot sem (Go n)       = go sem n
foldRobot sem TurnRight    = turnRight sem
foldRobot sem TurnLeft     = turnLeft sem
foldRobot sem (Seq r1 r2)  = seq_ sem (foldRobot sem r1) (foldRobot sem r2)
foldRobot sem (Repeat n r) = repeat_ sem n (foldRobot sem r)
```

### Subtask W6.1.1

Define semantics `stepsSem :: RobotSem Int` for calculating the total number of steps
(forwards and backwards) that the robot will go (turning does not count, only going).

### Subtask W6.1.2

Implement a function `squareDistance :: Robot -> Natural`
thaz calculates the _square_ of the distance (measured in steps)
from the starting point to the end point
for a given command in the `Robot`-DSL,

### Subtask W6.1.3

Consider the following simplified DSL for the same robot.
```
data SimpleRobot =
      SimpleRest
    | SimpleGo Int SimpleRobot
    | SimpleTurnRight SimpleRobot
    | SimpleTurnLeft SimpleRobot
```

Define semantics `simpleSem :: RobotSem SimpleRobot` which faithfully translate
a command from the `Robot`-DSL to the `SimpleRobot`-DSL.

### Subtask W6.1.4

Define the type `SimpleRobotSem d` which describes semantics of type `d`
for the `SimpleRobot`-DSL. Also define the corresponding catamorphism
`foldSimpleRobot :: SimpleRobotSem d -> SimpleRobot -> d` and
give semantics `simpleEnergySem :: SimpleRobotSem Int` for the calculation
of energy consumption.

### Subtask W6.1.5

Make `Robot` an instance of the `Arbitrary`-class and then write a
property `prop_energy_simpleEnergy :: Robot -> Property`
which checks that energy consumption for a `Robot`-command does not change
when we first translate to a `SimpleRobot`-command and calculate energy consumption
afterwards.

## W6.2 Sets of Integers

Consider the following DSL for describing _finite sets of integers_:

```
data FinSet =
      Interval Integer Integer
    | Union FinSet FinSet        -- ^ The union of the two given sets.
    | Intersection FinSet FinSet -- ^ The intersection of the two given sets.
```

### W6.2.1

Define type `FinSetSem d` to describe semantics of type `d` for the `FinSet`-DSL
and implement the corresponding catamorphism
`foldFinSet :: FinSetSem d -> FinSet -> d`.

### W6.2.2

Implement semantics `elemSem :: FinSetSem (Integer -> Bool)`
for `FinSet` which allow you to decide whether a given integer
is an element of the given set.

### W6.2.3

Implement semantics `minMaxSem :: FinSetSem (Maybe (Integer, Integer))`
that allow you to compute minimum and maximum of a `FinSet`
(or `Nothing` if the set is empty).

### W6.2.4

Define semantics `intSetSem :: FinSetSem IntSet` to convert a `FinSet` into the
`IntSet` (from `Data.IntSet`) representing the same set of integers.
