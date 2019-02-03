--main = putStrLn "hello, world"  
import Data.Char
-- digitToInt


main = do  
    print "[ I (Insert) | F (Find node) | Q (Quit) ] Command >"  
    cmd <- getLine
    
    -- create empty tree
    let tree = Node 5 ET ET ["Root's value!"]
    
    if cmd == "I" then do
        -- key
        k <- getChar
        let r = treeInsert (digitToInt k) ET "val"
        print r

    else if cmd == "F" then do
        print "Find key >"
        k <- getChar
        let op = treeElem (digitToInt k) tree -- search k from tree
        print op
        
    else do
        print $ "Invalid command, bye!"
        















        
        
        
        
        
-- key is type of int, just testing...
type Key = Int



-- Binary Tree (subtree) is either EMPTY (ET) OR has: key, left & right children and some val. list.
-- Stored values are Strings
data BT key = ET | Node Key (BT key) (BT key) [String] deriving(Show, Eq)


treeInsert key ET value = leaf key value
treeInsert x (Node key left right vs) newVal   
    | x == key  = Node x left right (vs ++ [newVal])
    | x < key   = Node key (treeInsert x left newVal) right vs
    | x > key   = Node key left (treeInsert x right newVal) vs

    
    
    
-- Function to return a Node with no children
leaf key value = Node key (ET) (ET) [value]
  


  
treeElem x ET = Nothing  
treeElem x (Node a left right vals)  
    | x == a = Just vals  
    | x < a  = treeElem x left   
    | x > a  = treeElem x right









{-
    http://learnyouahaskell.com/input-and-output
-}