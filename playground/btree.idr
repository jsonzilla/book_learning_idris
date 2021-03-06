module btree

data BTree a = Leaf
            | Node (BTree a) a (BTree a)

total insert : Ord a => a -> BTree a -> BTree a
insert x Leaf = Node Leaf x Leaf
insert x (Node l v r) = if (x < v) then (Node (insert x l) v r)
                                   else (Node l v (insert x r))

total toList : BTree a -> List a
toList Leaf = []
toList (Node l v r) = btree.toList l ++ (v :: btree.toList r)

total toTree : Ord a => List a -> BTree a
toTree [] = Leaf
toTree (x :: xs) = insert x (toTree xs)
