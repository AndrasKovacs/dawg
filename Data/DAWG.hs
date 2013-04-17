{-# LANGUAGE BangPatterns, PatternGuards, LambdaCase, TupleSections, RecordWildCards #-}


-- TODO:
    -- Test for Binary serialization


    -- ByteString.Word8, Char8, Text and String based lookup and search.
    -- Set
    -- Map with perfect hash. Individual and bulk modification.
    -- Map in ST monad. Inherit Vector functions essentially. 
    -- Compact DAWGS and random node access DAWGS too.
    -- One fast and one slower DAWG generation. 
    -- Binary monad instance for serialization
    -- use Vector more for traversal etc.
    -- generation: ST hashtables of Vectors with murmurhash?

    -- trie construction in ST with vectors and appends?
        -- In which case we could maybe skip the sorting requirement and sort on the go, with some optimized linear lookup. 

    -- toList should output sorted

module Data.DAWG (

    -- * Types
      Node

    -- * Construction
    , fromList
    , fromAscList
    , fromFile 

    -- * Accessors
    , value
    , endOfWord
    , getRoot
    , getChildren
    , lookupPrefixBy
    , lookupPrefix
    , elemBy
    , elem

    -- * Conversions
    , toList
    , toFile

    -- * Internal
    , pack
    , unpack 
    , NodeVector
    , nodeVector
    , endOfList
    , childIndex
    , getNodeAt
    ) where

import qualified Data.Vector.Unboxed as V
import qualified Data.IntMap as M 
import qualified Control.Monad.State as S
import Data.Binary
import Data.Vector.Binary
import Data.List (foldl', sort, find)
import Prelude hiding (elem)
import Data.Bits
import Data.Int
import Data.Char
import Text.Printf  


type NodeVector = V.Vector Int32


data Node = Node {
    {- UNPACK -} nodeVector :: !NodeVector,
    {- UNPACK -} childIndex :: !Int32, 
    {- UNPACK -} value      :: !Char, 
    {- UNPACK -} endOfList  :: !Bool, 
    {- UNPACK -} endOfWord  :: !Bool} 


instance Show Node where
    show (Node _ chi val eol eow) = printf 
        "{childIndex: %d, value: %c, endOfList: %s, endOfWord: %s}" 
        chi val (show eol) (show eow)


instance Binary Node where
    put (Node v chi val eol eow) = put (v, chi, val, eol, eow)
    get = do
        (v, chi, val, eol, eow) <- get
        return (Node v chi val eol eow)


pack :: Char -> Bool -> Bool -> Int -> Int32
pack !val !eol !eow !chi = 
    fromIntegral $ foldl' (.|.) 0 [
        chi `shiftL` 10, 
        ord val `shiftL` 2, 
        fromEnum eol `shiftL` 1, 
        fromEnum eow]


unpack :: Int32 -> NodeVector -> Node
unpack !n !v = Node {
    nodeVector = v,
    childIndex = (n .&. 4294966272) `shiftR` 10,
    value      = chr $ fromIntegral $ (n .&. 1020) `shiftR` 2,
    endOfList  = ((n .&. 2) `shiftR` 1) == 1,
    endOfWord  = (n .&. 1) == 1}


-- No boundschecking here because the the dawg generator never returns an empty vector. 
getRoot :: Node -> Node
getRoot !(Node{nodeVector=v}) = unpack (V.unsafeLast v) v


getNodeAt :: NodeVector -> Int32 -> Node
getNodeAt !v !i = unpack (V.unsafeIndex v (fromIntegral i)) v 


getChildren :: Node -> [Node] 
getChildren !(Node v chi _ _ _)
    | chi == 0  = [] -- The zero index is the end node by specification.
    | otherwise = go chi [] where
        go !i !acc | endOfList n = n:acc
                   | otherwise   =  go (i + 1) (n:acc) where 
                        n = getNodeAt v i


lookupPrefixBy :: (Char -> Char -> Bool) -> String -> Node -> Maybe Node
lookupPrefixBy p = go where
    go ![]     !n = Just n
    go !(x:xs) !n = maybe Nothing (go xs) (find ((p x) . value) (getChildren n))


lookupPrefix :: String -> Node -> Maybe Node
lookupPrefix = lookupPrefixBy (==)


elemBy :: (Char -> Char -> Bool) -> String -> Node -> Bool
elemBy p !xs !n = maybe False endOfWord $ lookupPrefixBy p xs n


elem :: String -> Node -> Bool
elem = elemBy (==)



-- ************* Construction *******************


data Trie = TrieNode {
    {- UNPACK -} eow :: !Bool,     
    {- UNPACK -} val :: !Char,    
    {- UNPACK -} chd :: ![Trie]}


insert :: String -> Trie -> Trie 
insert []     !n = n {eow = True}
insert (x:xs) !n@(TrieNode {chd = chd})
    | c:cs <- chd, 
      val c == x = n {chd = insert xs c :cs}
    | otherwise  = n {chd = insert xs (TrieNode False x []) :chd}


-- TODO: hashing packed child lists directly!
reduce :: Trie -> S.State (M.IntMap Int, [[Int32]], Int) (Int, Int32)
reduce !node@(TrieNode{..}) = do
    (hashes, cs) <- unzip `fmap` mapM reduce chd
    (chiMap, list, i) <- S.get

    let chunks = let f n = (n .&. 255): f (n `shiftR` 8::Int) in take 8 . f 
        getHash = foldl' (\s x -> 3074457345618258791 * (s + x))
        childHash = getHash 0 (chunks =<< hashes)
        nodeHash = getHash childHash [ord val, fromEnum eow]
        cs' | c:rest <- cs = (c .|. 2): rest -- set eol for last item
            | otherwise    = cs 
    
    ((nodeHash,) . pack val False eow) `fmap` maybe
        (S.put (M.insert childHash (i + 1) chiMap, cs':list, i + length cs') 
            >> return (i + 1))
        (return)
        (M.lookup childHash chiMap)


trieToNode :: Trie -> Node
trieToNode trie = let
    nullNodeState = (M.singleton 0 0, [[0]], 0)
    ((h, root), (_, l, _)) = S.runState (reduce trie) nullNodeState
    v = V.fromList $ reverse $ (root .|. 2): concat l
    in unpack (V.unsafeLast v) v 
    

mkTrie :: [String] -> Trie 
mkTrie = foldl' (flip insert) (TrieNode False '\0' [])

-- Sortedness not checked. 
fromAscList :: [String] -> Node
fromAscList = trieToNode . mkTrie 

fromList :: [String] -> Node
fromList = fromAscList . sort

fromFile :: FilePath -> IO Node
fromFile = decodeFile

toFile :: FilePath -> Node -> IO ()
toFile = encodeFile

toList :: Node -> [String]
toList n = ["" | endOfWord n] ++ (go =<< getChildren n) where 
    go n = [[value n] | endOfWord n] ++ (map (value n:) . go =<< getChildren n)
