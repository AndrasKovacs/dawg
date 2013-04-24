{- | 

Directed acyclic word graphs (DAWGs) are tries (see <http://en.wikipedia.org/wiki/Trie>) with merged identical nodes.

DAWGs can represent string sets very compactly. This implementation uses less than 1/100th of the space used by Data.Set for string dictionaries. 

However, no insertion or deletion can be practically done after the initial generation. 

This implementation stores a DAWG node in four bytes, using 22 bits for indexing and 8 bits for data storage. This implies that

    * The number of nodes shouldn't exceed 2^22, or 4194304.

    * Input characters should be mapped to the 0-255 range.

-}

{-# LANGUAGE BangPatterns, PatternGuards, LambdaCase, TupleSections, RecordWildCards #-}

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
import qualified Data.HashMap.Strict as HM
import qualified Control.Monad.State.Strict as S
import Control.DeepSeq
import Control.Arrow
import Data.Binary
import Data.Vector.Binary
import Data.List (foldl', sort, find)
import Prelude hiding (elem)
import Data.Bits
import Data.Word
import Data.Char
import Text.Printf

{-| 
The underlying container of the DAWG data. Modifying it will most likely result in an invalid DAWG.

Each "Word32" represents a node. The format of a node is the following:

    * 22 bits: the index of the first child.

    * 8 bits: character data.

    * 1 bit: end-of-word flag.

    * 1 bit: end-of-childlist flag.

The children of a node are laid out next to each other, so they can be iterated over by starting from the first child
and incrementing the index until a node with the end-of-childlist flag is found. 
-}
type NodeVector = V.Vector Word32

-- | This data type points to a prefix in the DAWG. When a node is the root node
-- it represents the whole DAWG. When it is non-root, it can be used to access the suffixes
-- of the prefix pointed to by the node. 
data Node = Node {
    -- | Get the underlying vector from a node.
    {- UNPACK -} nodeVector :: !NodeVector,
    -- | Get the index of a node's first child node. 
    {- UNPACK -} childIndex :: !Word32, 
    -- | Get the character value of a node. The root nodes have the null character as value. 
    {- UNPACK -} value      :: !Char, 
    -- | Indicates whether a node is the last in a list of children nodes. 
    {- UNPACK -} endOfList  :: !Bool, 
    -- | Indicates whether a prefix pointed to by the node is a valid word.
    {- UNPACK -} endOfWord  :: !Bool} deriving Eq 


instance Show Node where
    show (Node _ chi val eol eow) = printf 
        "{childIndex: %d, value: %c, endOfList: %s, endOfWord: %s}" 
        chi val (show eol) (show eow)


instance Binary Node where
    put (Node v chi val eol eow) = put (v, chi, val, eol, eow)
    get = do
        (v, chi, val, eol, eow) <- get
        return (Node v chi val eol eow)


instance NFData Node where
    rnf (Node v chi val eol eow) = 
        rnf v `seq` rnf chi `seq` rnf val `seq` rnf eol `seq` rnf eow `seq` ()

-- | Create a bit-packed Word32. 
pack :: Char -> Bool -> Bool -> Int -> Word32
pack !val !eol !eow !chi = 
    fromIntegral $ foldl' (.|.) 0 [
        chi `shiftL` 10, 
        ord val `shiftL` 2, 
        fromEnum eol `shiftL` 1, 
        fromEnum eow]
{- INLINE pack -}

-- | Create a node from a "Word32" and a "NodeVector". It is assumed that the "Word32" is
-- actually contained in the "NodeVector". 
unpack :: Word32 -> NodeVector -> Node
unpack !n !v = Node {
    nodeVector = v,
    childIndex = (n .&. 4294966272) `shiftR` 10,
    value      = chr $ fromIntegral $ (n .&. 1020) `shiftR` 2,
    endOfList  = ((n .&. 2) `shiftR` 1) == 1,
    endOfWord  = (n .&. 1) == 1}
{- INLINE unpack -}


-- | Get the root node from a node. 
getRoot :: Node -> Node
getRoot !(Node{nodeVector=v}) = unpack (V.unsafeLast v) v

-- | Create a node from some element of a "NodeVector". 
getNodeAt :: NodeVector -> Word32 -> Node
getNodeAt !v !i = unpack (V.unsafeIndex v (fromIntegral i)) v
{- INLINE getNodeAt -}

-- | Generate a list of the direct children of a node. 
getChildren :: Node -> [Node] 
getChildren !(Node v chi _ _ _)
    | chi == 0  = [] -- The zero index is the end node by specification.
    | otherwise = go chi [] where
        go !i !acc | endOfList n = n:acc
                   | otherwise   =  go (i + 1) (n:acc) where 
                        n = getNodeAt v i
{- INLINE getChildren -}

-- | Lookup a prefix by elementwise applying a comparison function. It is useful for
-- setting case sensitivity, e.g. @insensitiveLookup = lookupPrefixBy (comparing toLower)@
lookupPrefixBy :: (Char -> Char -> Bool) -> String -> Node -> Maybe Node
lookupPrefixBy p = go where
    go ![]     !n = Just n
    go !(x:xs) !n = maybe Nothing (go xs) (find ((p x) . value) (getChildren n))
{- INLINE lookupPrefixBy -}

-- | @lookupPrefix = lookupPrefixBy (==)@
lookupPrefix :: String -> Node -> Maybe Node
lookupPrefix = lookupPrefixBy (==)
{- INLINE lookupPrefix -}

-- | Test for membership with a comparison function. 
elemBy :: (Char -> Char -> Bool) -> String -> Node -> Bool
elemBy p !xs !n = maybe False endOfWord $ lookupPrefixBy p xs n
{- INLINE elemBy -}

-- | @elem = elemBy (==)@
elem :: String -> Node -> Bool
elem = elemBy (==)
{- INLINE elem -}


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


mkTrie :: [String] -> Trie 
mkTrie = foldl' (flip insert) (TrieNode False '\0' [])


-- | Read a DAWG previously serialized with "toFile" from a file. 
fromFile :: FilePath -> IO Node
fromFile = decodeFile

-- | Serialize a DAWG. 
toFile :: FilePath -> Node -> IO ()
toFile = encodeFile

-- | Get the list of all suffixes that end on a valid word ending. 
-- When used on the root node this function enlists the original words. The resulting list is unsorted.
toList :: Node -> [String]
toList n = ["" | endOfWord n] ++ (go =<< getChildren n) where 
    go n = [[value n] | endOfWord n] ++ (map (value n:) . go =<< getChildren n)


reduce :: Trie -> S.State (HM.HashMap [Word32] Int, Int) Word32
reduce !node@(TrieNode{..}) = do
    xs <- mapM reduce chd
    (chiMap, i) <- S.get
    let proc = \case []   -> (0, [])
                     [x]  -> (1, [x .|. 2])
                     x:xs -> (succ *** (x:)) (proc xs)
        (len, xs') = proc xs
    pack val False eow `fmap` maybe
        (S.put (HM.insert xs' (i + 1) chiMap, i + len) 
            >> return (i + 1))
        (return)
        (HM.lookup xs' chiMap)


trieToNode :: Trie -> Node
trieToNode trie = let
    (root, (m, i)) = S.runState (reduce trie) (HM.singleton [] 0, 0)
    assocs = (i + 1, root .|. 2): [(i', x) | (xs, i) <- HM.toList m, 
                                             (i', x) <- zip [i..] xs]
    vec = V.unsafeAccum (flip const) (V.replicate (i + 2) 0) assocs
    in unpack (V.unsafeLast vec) vec

-- | Allows for faster DAWG generation than "fromList". The ordering assumption is unchecked and a violation leads to invalid output. 
fromAscList :: [String] -> Node
fromAscList = trieToNode . mkTrie 

fromList :: [String] -> Node
fromList = fromAscList . sort