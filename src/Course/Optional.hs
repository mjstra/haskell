{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Optional where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Course.Core
import qualified Prelude as P

-- | The `Optional` data type contains 0 or 1 value.
--
-- It might be thought of as a list, with a maximum length of one.
data Optional a =
  Full a
  | Empty
  deriving (Eq, Show)

-- | Map the given function on the possible value.
--
-- >>> mapOptional (+1) Empty
-- Empty
--
-- >>> mapOptional (+1) (Full 8)
-- Full 9
mapOptional :: (a -> b)  -> Optional a  -> Optional b
mapOptional   _ Empty = Empty
mapOptional f (Full a) = Full(f a) 

--You'll see that the first argument is (a -> b), which is just a 
--function that takes an a and returns a b. The second argument is [a], which is a 
--list of values of type a, and the return type [b], a list of values of type b. So in 
--plain english, the map function applies a function to each element in a list of values, 
--then returns the those values as a list.


-- | Bind the given function on the possible value.
--
-- >>> bindOptional Full Empty
-- Empty
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8)
-- Full 7
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 9)
-- Full 10
bindOptional :: (a -> Optional b) -> Optional a -> Optional b
bindOptional _ Empty = Empty
bindOptional f (Full a) = f a


-- | Return the possible value if it exists; otherwise, the second argument.
--
-- >>> Full 8 ?? 99
-- 8
--
-- >>> Empty ?? 99
-- 99
(??) :: Optional a -> a -> a
Empty ?? a = a
Full a ?? _ = a 
-- | Try the first optional for a value. If it has a value, use it; otherwise,
-- use the second value.
--
-- >>> Full 8 <+> Empty
-- Full 8
--
-- >>> Full 8 <+> Full 9
-- Full 8
--
-- >>> Empty <+> Full 9
-- Full 9
--
-- >>> Empty <+> Empty
-- Empty

-- (<+>) :: Optional a -> Optional a -> Optional a
-- Empty <+> Full a = Full a
-- Full a <+> Full _ = Full a
-- Empty <+> Empty = Empty
-- Full a <+> Empty = Full a
--easier
(<+>) :: Optional a -> Optional a -> Optional a
--(<+>) Empty x = x
--(<+>) Full a _ = Full a
Empty <+> x = x
Full a <+> _ = Full a

-- | Replaces the Full and Empty constructors in an optional.
--
-- >>> optional (+1) 0 (Full 8)
-- 9
-- >>> optional (+1) 0 Empty
-- 0
-- >>>           f   a Empty
optional :: (a -> b) -> b -> Optional a -> b
optional f _ (Full b) = f b
optional _ e Empty = e


applyOptional :: Optional (a -> b) -> Optional a -> Optional b
applyOptional o f = bindOptional (\f' -> mapOptional f' f) o

twiceOptional :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f = applyOptional . mapOptional f

contains :: Eq a => a -> Optional a -> Bool
contains _ Empty = False
contains a (Full z) = a == z

instance P.Functor Optional where
  fmap =
    M.liftM

instance A.Applicative Optional where
  (<*>) =
    M.ap
  pure =
    Full

instance P.Monad Optional where
  (>>=) =
    flip bindOptional
  return =
    Full
