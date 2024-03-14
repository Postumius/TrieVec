# TrieVec
based on [this blogpost](https://hypirion.com/musings/understanding-persistent-vector-pt-1) explaining Clojure's vector type

## How It Works (Roughly) 
It's a seach tree that only stores data in the leaves. The branching factor can be any power of two, and can be set by redefining the constant ```p``` in ```Vec.hs```, but for now let's assume it's 2. To lookup an index, we go through its binary digits from left to right (I actually go backwards throught the digits for implementation reasons, but don't worry about that now), and every time we see a 0 we go into the left subtree, and when we see a 1 we go right. For example, if we're looking up index 6, that's 110, so from the root of the tree we go right -> right -> left down the tree and reach a leaf. We grow the tree by plopping a new root on top, so that the old tree is now the leftmost subtree. In our example, after having grown past 7 elements we use the representation 0110 for 6, so that we go into the left subtree first. For someone else's much better explanation *with pictures*, see the aforementioned blogpost.

## Performance and Functionality
Push, pop, and index lookup all take O(log_b(n)) time, where b the branching factor. Doesn't currently implement the tail optimization (I'll add that in the future). For every push, a new path from root to leaf is created, so after n pushes the Vector will take up O(nlog(n)) space. 

The vector is an instance of Foldable, Functor, and Monoid, so it can be folded, mapped over, and appended to. Traversal is O(n) but appending is basically just repeated pushing, so it's O(mlog(n+m)), where n is the size of the left vector and m is the size of the right.
