{-# LANGUAGE NamedFieldPuns #-}

module W0603
    ( -- proposition
      Prop (..), PropSem (..), foldProp, Env, empty, extend, evalSem, varsSem
      -- * Subtask W6.3.1
    , sat
      -- * Subtask W6.3.2
    , nand, NandSem (..), foldNand, evalSem', nandSem
    ) where

import           Data.Set (Set)
import qualified Data.Set as S

-- | A DSL for propositions.
data Prop =
      Var String
    | T
    | F
    | Not Prop
    | And Prop Prop
    | Or Prop Prop
    deriving (Show, Eq)

-- | Describes semantics of type @d@ for 'Prop's.
data PropSem d = PropSem
    { var_ :: String -> d
    , t_   :: d
    , f_   :: d
    , not_ :: d -> d
    , and_ :: d -> d -> d
    , or_  :: d -> d -> d
    }

-- | Catamorphism for 'Prop's.
--
-- >>> foldProp (PropSem Var T F Not And Or) $ Var "x" `And` (Not (Var "y" `Or` T))
-- And (Var "x") (Not (Or (Var "y") T))
--
foldProp :: PropSem d -> Prop -> d
foldProp PropSem{ var_, t_, f_, not_, and_, or_ } = go
  where
    go (Var s)   = var_ s
    go T         = t_
    go F         = f_
    go (Not p)   = not_ $ go p
    go (And p q) = and_ (go p) (go q)
    go (Or p q)  = or_ (go p) (go q)

-- | Environment to assign truth values to variables.
type Env = String -> Bool

-- | The "empty" 'Env', assigning 'False' to all variables.
empty :: Env
empty = const False

-- | Extends an environment by assigning the given truth value to
-- the variable with the given name.
extend :: String -> Bool -> Env -> Env
extend s b env = \s' -> if s' == s then b else env s'

-- | Defines semantics to evaluate a 'Prop' in an 'Env'.
evalSem :: PropSem (Env -> Bool)
evalSem = PropSem
    { var_ = flip ($)
    , t_   = const True
    , f_   = const False
    , not_ = (not .)
    , and_ = \p q env -> p env && q env
    , or_  = \p q env -> p env || q env
    }

-- | Defines semantics to get the set of variables mentioned in a 'Prop'.
varsSem :: PropSem (Set String)
varsSem = PropSem
    { var_ = S.singleton
    , t_   = S.empty
    , f_   = S.empty
    , not_ = id
    , and_ = S.union
    , or_  = S.union
    }

-- Subtask W6.3.1

-- | Checks whether the given 'Prop' is /satisfiable/, i.e. whether
-- there exists an environment in which the proposition evaluates to 'True'.
-- If yes, 'Just' such an environment is returned. If no, 'Nothing' is returned.
--
-- >>> let Just env = sat (Var "x" `And` (Not $ Var "y")) in env <$> ["x", "y"]
-- [True,False]
-- >>> const () <$> sat (Var "x" And (Not $ Var "x"))
-- Nothing
--
sat :: Prop -> Maybe Env
sat = error "TODO: implement sat"

-- Subtask W6.3.2

-- | The "not and" operation on 'Bool'.
nand :: Bool -> Bool -> Bool
nand x y = not $ x && y

-- | DSL for proposition expressed as variables combined with 'nand'.
data Nand = Var' String | Nand Nand Nand deriving (Show, Eq)

data NandSem d = NandSem
    { -- TODO: define NandSem
    }

-- | Catamorphism for the 'Nand'-DSL.
--
-- >>> foldNand (NandSem Var' Nand) $ Var' "x" `Nand` (Var' "y" `Nand` Var' "z")
-- Nand (Var' "x") (Nand (Var' "y") (Var' "z"))
--
foldNand :: NandSem d -> Nand -> Env -> d
foldNand = error "TODO: implement foldNand"

-- | Describes the semantics of evaluating a proposition given in the 'Nand'-DSL
-- in an environment.
--
-- >>> foldNand evalNandSem (Var' "x" `Nand` Var' "x") empty
-- True
-- >>> foldNand evalNandSem (Var' "x" `Nand` (Var' "x" `Nand` Var' "y") empty
-- True
-- >>> foldNand evalNandSem (Var' "x" `Nand` (Var' "x" `Nand` Var' "y") $ extend "x" True empty
-- False
--
evalSem' :: NandSem (Env -> Bool)
evalSem' = error "TODO: implement evalSem'"

-- | Describes the semantics of converting a proposition expressed in the 'Prop'-DSL
-- into an equivalent 'Nand'-form.
--
-- >>> let bs   = [True, False]
-- >>> let envs = [extend "x" x $ extend "y" y $ extend "z" z empty | x <- bs, y <- bs, z <- bs]
-- >>> let p    = Not (Var "x") `And` (Var "y" `Or` Not (Var "z"))
-- >>> let p'   = foldProp nandSem p
-- >>> let bs   = foldProp evalSem  p  <$> envs
-- >>> let bs'  = foldNand evalSem' p' <$> envs
-- >>> bs == bs'
-- True
--
nandSem :: PropSem Nand
nandSem = error "TODO: implement nandSem"
