module W0602
    ( -- finite sets of integers
      FinSet (..)
      -- * Subtask W6.2.1
    , FinSetSem (..), foldFinSet
      -- * Subtask W6.2.2
    , elemSem
      -- * Subtask W6.2.3
    , minMaxSem
      -- * Subtask W6.2.4
    , intSetSem
    ) where

import Data.Set (Set)

-- | A DSL for describing finite sets of integers.
data FinSet =
      Interval Integer Integer   -- ^ @'Interval' l u@ denotes the set of all @n@ with @l <= n <= u@.
    | Union FinSet FinSet        -- ^ The union of the two given sets.
    | Intersection FinSet FinSet -- ^ The intersection of the two given sets.
    deriving (Show, Eq)

-- Subtask W6.2.1

-- | Data defining semantics of type @d@ for the 'FinSet'-DSL.
data FinSetSem d = FinSetSem
    { -- TODO: Define the fields of FinSetSem
    }

-- | Catamorphism for the 'FinSet'-DSL.
--
-- >>> foldFinSet (FinSetSem Interval Union Intersection) $ Interval 1 10 `Union` (Interval 20 30 `Intersection` Interval 25 35)
-- Union (Interval 1 10) (Intersection (Interval 20 30) (Interval 25 35))
--
foldFinSet :: FinSetSem d -> FinSet -> d
foldFinSet = error "TODO: implement foldFinSet"

-- Subtask W6.2.2

-- | Provides 'FinSet's with semantics to decide whether a given 'Integer'
-- is an element of the set.
--
-- >>> foldFinSet elemSem (Interval 7 15) 10
-- True
-- >>> foldFinSet elemSem (Interval 10 9 `Union` Interval 1 8) 2
-- True
-- >>> foldFinSet elemSem (Interval 1 4 `Intersection` Interval 5 10) 5
-- False
--
elemSem :: FinSetSem (Integer -> Bool)
elemSem = error "TODO: implement elemSem"

-- Subtask W6.2.3

-- | Defines semantics to compute minimum and maximum of a 'FinSet'
-- (or 'Nothing' if the set is empty).
--
-- >>> foldFinSet minMaxSem $ Interval 1 10 `Union` Interval 20 30
-- Just (1,30)
-- >>> foldFinSet minMaxSem $ Interval 1 10 `Intersection` Interval 20 30
-- Nothing
-- >>> foldFinSet minMaxSem $ Interval 1 10 `Intersection` Interval 5 11
-- Just (5,10)
--
minMaxSem :: FinSetSem (Maybe (Integer, Integer))
minMaxSem = error "TODO: implement minMaxSem"

-- Subtask W6.2.4

-- | Defines semantics to convert a 'FinSet' into the @'Set' 'Integer'@ representing
-- the same set of 'Integer's.
--
-- >>> foldFinSet intSetSem $ Interval 1 5 `Intersection` (Interval 0 1 `Union` (Interval 4 20))
-- fromList [1,4,5]
--
intSetSem :: FinSetSem (Set Integer)
intSetSem = error "TODO: implement intSetSem"
