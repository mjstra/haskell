--


compose
>> f = (*2)
>> g = (+3)
>> (f . g) 5
16
>> f . g 5
error
>> z = (*)
>> z 3 4
12
>> :t f . z 5
f . z :: (Num a, Num (a -> a)) => a -> a -> a
>> :t f . z 5
f . z 5 :: Num c => c -> c
>>
>> (f . z) 5 --TODO: compose expects two unary functions!! z is binary (*): a,b -> a * b
<interactive>:344:1: error:
    • Non type-variable argument in the constraint: Num (a -> a)
      (Use FlexibleContexts to permit this)

>> f . z 5 $ 3 --z basically becomes unary / uncurried
30 -- (*2 . *5) 3
>> f . z 5 3
-- doesn't work because tries to apply 3 to 5 ? 
>> f . z ((+5) 3) $ 2
32


{-
instance Applicative ((->) t) where
  pure :: a -> ((->) t a)
  pure = const 
-}

-- a class functor, instance applicative that waits
lift0 :: Applicative k => a -> k a
lift0 = pure

-- >>> lift1 (+1) (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
lift1 :: Applicative k => (a -> b) -> k a -> kb
--lift1 f a = lift0 f <*> a
lift1 = (<$>) --damn so simple...
--<$> is infix lift1

>> lift1 (+1) (Full 8)
Full 9
>>  lift0 (+1) <*> Full 8
Full 9

meanwhile
>> const <$> (Full 5) <*> (Full 2)
Full 5
>> const id <$> (Full 5) <*> (Full 2)
Full 2
>> const id (Full 2) (Full 4)
Full 4

--TODO:
(*>):: Applicative k => (a -> b) -> k a -> k b
(*>) f a = const id <$> a <*> b
--or f a = id <$ a <*> b

(<*):: Applicative k => (a -> b) -> k a -> k b
(<*) f a = id <$ b <*> a 
(<*) = lift2 const--id <$ b <*> a


sequence ::Applicative k => List (k a) -> k (List a)
                  --lift (:.): gets two arguments (acc, next)
                  --acc starts with pure Nil (Empty Functor) Full (), [], etc
sequence = foldRight (lift2 (:.)) (pure Nil)

replicateA ::Applicative k => Int-> k a -> k (List a)
replicateA n = sequence . replicate n --const <$ a  sequence (foldRight lift2(:.) (pure:.Nil)) 
       --TODO: is not the same as     
       =! (sequence . replicate) n
        
-- or 
-- sequence = foldRight (\acc next -> (:.) <$> acc <*> next) (pure Nil) 
