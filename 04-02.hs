{-
    Make your Null  a functor.
-}

{- 
    I have no idea if this is what was requested...
    In this case input is MaybeNull and so is output

    Test:
    fmap (*2) (Value 100)
    > Value 200
    
    fmap (\p -> p+1) (Value 1)
    > Value 2
    
    fmap (+1) Null
    > Null
-}



-- Custom type
data MaybeNull v = Null | Value v               deriving(Show)


instance Functor (MaybeNull) where
    fmap f (Value z) = Value $ f z
    fmap f Null = Null