{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

-- | Mutable data structures, etc.

module Feldspar.Mutable
  ( module Mutable
  , Buffer (..)
  , initBuffer'
  , initBuffer
  , newBuffer
  , newBuffer_
  , tM
  , initBuffer2

  -- * Mutable multidimensional arrays
  , MDArr (..)
  , newMDArr
  , getMDArr
  , setMDArr
  , storableToMDArr
  , freezeMDArr
  ) where

import qualified Prelude

import Feldspar
import Feldspar.Core.Language                  as Mutable
import Feldspar.Vector

-- | Indexable cyclic buffer
data Buffer a = Buffer
    { indexBuf :: Data Index -> M a
    , putBuf   :: a -> M ()
    , withBuf  :: forall b . Syntax b => (Pull DIM1 a -> M b) -> M b
    }

-- Another option would be to represent a buffer as its state (the counter and the array), but the
-- above representation leaves room for other implementations.

--- | Create a new cyclic buffer
initBuffer' :: forall a . Syntax a => Data (MArr (Internal a)) -> M (Buffer a)
initBuffer' buf = do
    l  <- arrLength buf
    ir <- newRef 0
    let get j = do
          i <- getRef ir
          fmap sugar $ getArr buf $ calcIndex l i j
        put a = do
          i <- getRef ir
          setRef ir ((i+1) `mod` l)
          setArr buf i $ desugar a
        with :: Syntax b => (Pull DIM1 a -> M b) -> M b
        with f = do
          i <- getRef ir
          withArray buf (f . freeze i)
    return (Buffer get put with)
  where
    calcIndex l i j = (l+i-j-1) `mod` l

    freeze :: Syntax b => Data Index -> Data [Internal b] -> Pull DIM1 b
    freeze i = permute (\l -> calcIndex l i) . map sugar . thawPull1

-- | Create a new cyclic buffer initalized by the given vector (which also determines the size)
initBuffer :: Syntax a => Pull DIM1 a -> M (Buffer a)
initBuffer buf = thawArray (freezePull1 $ map desugar buf) >>= initBuffer'

-- | Create a new cyclic buffer of the given length initialized by the given element
newBuffer :: Syntax a => Data Length -> a -> M (Buffer a)
newBuffer l init = newArr l (desugar init) >>= initBuffer'

-- | Create a new cyclic buffer of the given length without initialization
newBuffer_ :: Syntax a => Data Length -> M (Buffer a)
newBuffer_ l = newArr_ l >>= initBuffer'

tM :: Patch a a -> Patch (M a) (M a)
tM _ = id

initBuffer2' :: forall a . Syntax a => Data Length -> Data (MArr (Internal a))
                                    -> M (Buffer a)
initBuffer2' l buf = do
    ir <- newRef 0
    let get j = do
          i <- getRef ir
          fmap sugar $ getArr buf (j + i)
        put a = do
          i <- getRef ir
          setRef ir ((i+1) `mod` l)
          let a' = desugar a
          setArr buf i a'
          setArr buf (i+l) a'
        with :: Syntax b => (Pull DIM1 a -> M b) -> M b
        with f = do
          i <- getRef ir
          withArray buf (f . freeze i)
    return (Buffer get put with)
  where
    freeze :: Syntax b => Data Index -> Data [Internal b] -> Pull DIM1 b
    freeze i = take l . drop i . map sugar . thawPull1

-- | Create a new cyclic buffer. This implementation uses a buffer twice
--   as long as necessary to avoid all modulus operations when accessing
--   the elements.
initBuffer2 :: Syntax a => Pull DIM1 a -> M (Buffer a)
initBuffer2 buf = thawArray (freezePush1 $ dup $ map desugar buf) >>=
                  initBuffer2' (length buf)

-- Mutable multidimensional arrays

data MDArr sh a = MDArr (Data (MArr (Internal a))) (Shape sh)

newMDArr :: Syntax a => Shape sh -> a -> M (MDArr sh a)
newMDArr sh a = do arr <- newArr (size sh) (desugar a)
                   return (MDArr arr sh)

getMDArr :: Syntax a => MDArr sh a -> Shape sh -> M a
getMDArr (MDArr marr sh) shi = do
  let i = toIndex shi sh
  fmap sugar (getArr marr i)

setMDArr :: Syntax a => MDArr sh a -> Shape sh -> a -> M ()
setMDArr (MDArr marr sh) shi a = do
  let i = toIndex shi sh
  setArr marr i (desugar a)

storableToMDArr :: (Storable vec, VecShape vec ~ sh, Syntax a)
                => vec a -> M (MDArr sh a)
storableToMDArr vec = do marr <- thawArray arr
                         return (MDArr marr sh)
  where Manifest arr sh = store vec

freezeMDArr :: (Syntax a) => MDArr sh a -> M (Manifest sh a)
freezeMDArr (MDArr marr sh) = do
  arr <- freezeArray marr
  return (Manifest arr sh)
