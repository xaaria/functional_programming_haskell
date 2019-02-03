{-
    Define a typeclass for 3-valued equality (True, False, Unknown). 
    Make a type constructor MaybeNull (like database Null) which either contains a value or Null.
    Make MaybeNull a member of your 
    three-valued equality (comparison of Null with something would give Unknown).
-}

{-
    Test:
    Null `comp` Null
    > Unknown
    Null `comp` (Value 999)
    > Unknown
-}

-- Really have no Idea how to really implement that 3-valued
-- custom type that has True & False, since they are built-in in Haskell
-- but it does what was asked, custom type that is part of custom Typeclass, that
-- implements comparison method and return custom type  Unknown when comparing 
-- Null with something/anything




-- Possible return types
data CompRes = True | False | Unknown           deriving(Show)

-- Our custom type
data MaybeNull v = Null | Value v               deriving(Show)


class ThreeEq a where
    comp :: a -> a -> CompRes
    -- (/=) :: a -> a -> Bool
    
    
-- Make MaybeNull a member of your three-valued equality
instance ThreeEq (MaybeNull v) where
    Null `comp` _           = Unknown
    _ `comp` Null           = Unknown