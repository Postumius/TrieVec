#!/usr/bin/env cabal
{- cabal:
build-depends: base, array, integer-logarithms
-}

module Vec (
    size,
    empty,
    add,
    trim,
    trimN,
    fromList,
    toList,
    get,
    Vec.last,
    fmap,
    (<>),
    mempty,
    mappend,
    showsPrec
) where
    
import Data.Array
import Data.Maybe
import Math.NumberTheory.Logarithms ( integerLogBase )
import Data.Bits ( Bits((.&.), shiftR) )
import Control.Monad ( (>=>) )

-- branching factor of prefix tree is 2^p
p :: Int
p = 3

data Node a =
    Nil |
    Leaf a |
    Inner (Array Int (Node a))

data Vec a = Vec {
    size :: Int,
    tree :: Node a
} 

empty :: Vec a
empty = Vec 0 Nil

getHeight :: Int -> Int -> Int
getHeight _ 0 = 0
getHeight _ 1 = 0
getHeight b n = (+1) $ integerLogBase (toInteger b) $ toInteger (n-1)

isPowerOf :: Int -> Int -> Bool
isPowerOf n b = n == b ^ getHeight b n 

updateArray :: (Ix i) => 
    i -> (a -> a) -> Array i a -> Array i a
updateArray i f arr = arr // [(i, f $ arr ! i)]

getIndex :: Int -> Int -> Int
getIndex n h = 
    let b = 2^p
        shifted = shiftR n (p*(h-1))
    in (b-1) .&. shifted

add :: a -> Vec a -> Vec a 
add x (Vec n root) = 
    let branch = 2 ^ p
        height = getHeight branch n
        makeInner = Inner . listArray (0, branch-1)
        makeNodes h 
            | h <= 0 = Leaf x 
            | otherwise = 
                makeInner $
                makeNodes (h-1) : repeat Nil
        search h Nil = makeNodes h
        search h (Inner arr) = 
            let i = getIndex n h 
            in Inner $ updateArray i (search $ h-1) arr
    in Vec (n+1) $ 
        if n `isPowerOf` branch 
            then makeInner $ 
                root : 
                makeNodes height : 
                repeat Nil
            else search height root

trim :: Vec a -> Maybe (Vec a)
trim (Vec 0 nil) = Nothing
trim (Vec n root) = 
    let recur _ (Leaf _) = Nil 
        recur h (Inner arr) = 
            let i = getIndex (n-1) h
                arr' = updateArray i (recur $ h-1) arr
            in case arr' ! 0 of 
                Nil -> Nil 
                _ -> Inner arr' 
    in Just $ Vec (n-1) $ 
         case recur (getHeight (2^p) n) root of 
            Inner arr ->
                case arr ! 1 of 
                    Nil -> arr ! 0
                    _ -> Inner arr
            root' -> root'

trimN :: Int -> Vec a -> Maybe (Vec a)
trimN 0 = pure
trimN n = trimN (n-1) >=> trim

fromList :: [a] -> Vec a
fromList = foldr add empty  . reverse

instance Foldable Vec where
    foldr f acc (Vec _ root) =
        let recur Nil acc = acc 
            recur (Leaf x) acc = f x acc
            recur (Inner arr) acc = 
                foldr recur acc arr 
        in recur root acc

toList :: Vec a -> [a]
toList = foldr (:) []

get :: Int -> Vec a -> Maybe a
get i (Vec n root) = 
    let search _ (Leaf x) = Just x 
        search _ Nil = Nothing
        search h (Inner arr) = 
            search 
                (h-1)
                (arr ! getIndex i h)
    in search (getHeight (2^p) n) root

last :: Vec a -> Maybe a
last vec = get (size vec -1) vec 

instance Functor Vec where 
    fmap f (Vec n root) =
        let recur _ (Leaf x) = Leaf $ f x
            recur _ Nil = Nil
            recur h (Inner arr) = 
                Inner $ fmap (recur $ h-1) arr
        in Vec n $ 
            recur (getHeight p n) root

instance Semigroup (Vec a) where
    (<>) = foldl (flip add)

instance Monoid (Vec a) where 
    mempty = empty

    mappend = (<>)

instance (Show a) => Show (Vec a) where 
    showsPrec d vec = 
        (++) $
        (\s -> if d >= 10 
            then "(" ++ s ++ ")"
            else s) $
        ("fromList " ++) $
        show $
        toList vec
