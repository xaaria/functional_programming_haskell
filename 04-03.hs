

-- 03-03.hs

{-
    Create a binary tree type that stores (key, [value]) pairs. 
    Inserting with existing key appends to the key's list and search would give the whole list as an answer, 
    if an answer is found. 
    Return a Maybe value when searching. (Just value if found, Nothing otherwise).
-}

-- ET stands for "EmptyTree"


{-
    == How to test ==
    For example:
    
    [1] let r = Node 10 (ET) (ET) ["value1"]
    [2] let t2 = treeInsert 15 r "val2"
    [3] let t3 = treeInsert 15 t2 "val3"
    [4] treeElem 15 t3
    [5] treeElem 1000 t3
    
    
    [1] Create new binary tree. root is r. It has value 'value1' and empty children.
    [2] insert node with key 15. Start from root. new value as 3. param.
        (the result is saved in t2)
    [3] Try to add another Node with key 15. Since it already exists, 
        15's value list is appended instead
    [4] Search for key 15. Returns Just ["val2", "val3"]. param t3 is root.
    [5] Search for key 1000. Returns Nothing
    

    
-}


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