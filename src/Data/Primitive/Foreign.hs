--------------------------------------------------------------------------------

{-# language BangPatterns #-}
{-# language ForeignFunctionInterface #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

--------------------------------------------------------------------------------

module Data.Primitive.Foreign
  (
    -- * Prim-Storable methods
    sizeOf, alignment
  , peek, peekElemOff, peekByteOff
  , poke, pokeElemOff, pokeByteOff

    -- * Memory allocation
    -- ** Local allocation
  , alloca, F.allocaBytes, F.allocaBytesAligned
    -- ** Dynamic allocation
  , malloc, F.mallocBytes
  , calloc, F.callocBytes
  , realloc, F.reallocBytes
  , F.free, F.finalizerFree

    -- * Marshalling arrays
    -- ** Allocation
  , mallocArray, mallocArray0
  , allocaArray, allocaArray0
  , reallocArray, reallocArray0
  , callocArray, callocArray0
    -- ** Marshalling
  , peekArray, peekArray0
  , pokeArray, pokeArray0
    -- ** Combined allocation and marshalling
  , newArray, newArray0
  , withArray, withArray0
  , withArrayLen, withArrayLen0
    -- ** Copying
  , copyArray
  , moveArray
    -- ** Finding the length
  , lengthArray0
    -- ** Indexing
  , advancePtr

    -- * General marshalling utilities
    -- ** Combined allocation and marshalling
  , with
  , new
    -- ** Marshalling of Maybe values
  , F.maybeNew
  , F.maybeWith
  , F.maybePeek
    -- ** Haskellish interface to memcpy and memmove
  , F.copyBytes
  , F.moveBytes
    -- ** Filling up memory areas with required values
  , F.fillBytes
  ) where

--------------------------------------------------------------------------------

import Data.Coerce (coerce)
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Data.Void (Void)
import GHC.Ptr

import qualified Foreign as F

--------------------------------------------------------------------------------

-- [Section: Foreign.Storable]

-- | Read a value from a memory area regarded as an array of values
--   of the same kind. The first argument specifies the start address
--   of the array and the second the index into the array (the first
--   element of the array has index @0@). The following equality holds,
--
--   > peekElemOff addr idx = fixIO $ \result ->
--   >   peek (addr `plusPtr` (idx * sizeOf result))
--
--   Note that this is only a specification, not
--   necessarily the concrete implementation of the
--   function.
peekElemOff :: forall a. Prim a => Ptr a -> Int -> IO a
peekElemOff !ptr = coerce (F.peekElemOff @(PrimStorable a) (castPtr ptr))

-- | Write a value to a memory area regarded as an array of
--   values of the same kind. The following equality holds:
--
--   > pokeElemOff addr idx x =
--   >   poke (addr `plusPtr` (idx * sizeOf x)) x
pokeElemOff :: forall a. Prim a => Ptr a -> Int -> a -> IO ()
pokeElemOff !ptr !idx a = F.pokeElemOff (castPtr ptr) idx (PrimStorable a)

-- | Read a value from a memory location given by a base
--   address and offset. The following equality holds:
--
--   > peekByteOff addr off = peek (addr `plusPtr` off)
peekByteOff :: forall a. Prim a => Ptr Void -> Int -> IO a
peekByteOff !ptr = coerce (F.peekByteOff @(PrimStorable a) ptr)

-- | Write a value to a memory location given by a base
--   address and offset. The following equality holds:
--
--   > pokeByteOff addr off x = poke (addr `plusPtr` off) x
pokeByteOff :: forall a. Prim a => Ptr Void -> Int -> a -> IO ()
pokeByteOff !ptr !idx a = F.pokeByteOff ptr idx (PrimStorable a)

-- | Read a value from the given memory location.
--
--   Note that the peek and poke functions might require properly
--   aligned addresses to function correctly. This is architecture
--   dependent; thus, portable code should ensure that when peeking
--   or poking values of some type @a@, the alignment constraint for
--   @a@, as given by the function 'alignment' is fulfilled.
peek :: forall a. Prim a => Ptr a -> IO a
peek = coerce (F.peek . castPtr @a @(PrimStorable a))

-- | Write the given value to the given memory location. Alignment
--   restrictions might apply; see 'peek'.
poke :: forall a. Prim a => Ptr a -> a -> IO ()
poke !ptr a = F.poke (castPtr ptr) (PrimStorable a)

--------------------------------------------------------------------------------

-- [Section: Foreign.Marshal.Alloc]

-- |@'alloca' f@ executes the computation @f@, passing as argument
-- a pointer to a temporarily allocated block of memory sufficient to
-- hold values of type @a@.
--
-- The memory is freed when @f@ terminates (either normally or via an
-- exception), so the pointer passed to @f@ must /not/ be used after this.
alloca :: forall a b. Prim a => (Ptr a -> IO b) -> IO b
alloca f = F.alloca (coerce f :: Ptr (PrimStorable a) -> IO b)

malloc :: forall a. Prim a => IO (Ptr a)
malloc = F.mallocBytes (sizeOf @a undefined)

calloc :: forall a. Prim a => IO (Ptr a)
calloc = F.callocBytes (sizeOf @a undefined)

realloc :: forall a b. Prim b => Ptr a -> IO (Ptr b)
realloc ptr = coerce (F.realloc @a @(PrimStorable b) ptr)

--------------------------------------------------------------------------------

-- [Section: Foreign.Marshal.Array]

-- | Allocate storage for the given number of elements of a storable type
--   (like 'malloc', but for multiple elements).
mallocArray :: forall a. Prim a => Int -> IO (Ptr a)
mallocArray !idx = castPtr <$> F.mallocArray @(PrimStorable a) idx

-- | Like 'mallocArray', but add an extra position to hold a special
--   termination element.
mallocArray0 :: forall a. Prim a => Int -> IO (Ptr a)
mallocArray0 !idx = castPtr <$> F.mallocArray0 @(PrimStorable a) idx

-- | Temporarily allocate space for the given number of elements
--   (like 'alloca', but for multiple elements).
allocaArray :: forall a b. Prim a => Int -> (Ptr a -> IO b) -> IO b
allocaArray !idx f = F.allocaArray idx (coerce f :: Ptr (PrimStorable a) -> IO b)

-- | Like 'allocaArray', but add an extra position to hold a special
--   termination element.
allocaArray0 :: forall a b. Prim a => Int -> (Ptr a -> IO b) -> IO b
allocaArray0 !idx f = F.allocaArray0 idx (coerce f :: Ptr (PrimStorable a) -> IO b)

-- | Adjust the size of an array.
reallocArray :: forall a. Prim a => Ptr a -> Int -> IO (Ptr a)
reallocArray !ptr !idx = coerce (F.reallocArray @(PrimStorable a) (castPtr ptr) idx)

-- | Adjust the size of an array, including an extra position for the
--   terminating element.
reallocArray0 :: forall a. Prim a => Ptr a -> Int -> IO (Ptr a)
reallocArray0 !ptr !idx = coerce (F.reallocArray0 @(PrimStorable a) (castPtr ptr) idx)

-- | Like 'mallocArray', but allocated memory is filled with bytes of value zero.
callocArray :: forall a. Prim a => Int -> IO (Ptr a)
callocArray !idx = coerce (F.callocArray @(PrimStorable a) idx)

-- | Like 'mallocArray0', but allocated memory is filled with bytes of value zero.
callocArray0 :: forall a. Prim a => Int -> IO (Ptr a)
callocArray0 !idx = coerce (F.callocArray0 @(PrimStorable a) idx)

-- | Convert an array of given length into a Haskell 'PrimArray'.
peekArray :: forall a. Prim a => Int -> Ptr a -> IO (PrimArray a)
peekArray !sz !ptr = if sz <= 0
  then pure mempty
  else do
    marr <- newPrimArray sz
    let go !ix = if ix < sz
          then do
            writePrimArray marr ix =<< peekElemOff ptr ix
            go (ix + 1)
          else pure ()
    go 0
    unsafeFreezePrimArray marr

-- | Convert an array terminated by the given terminator into a Haskell
-- 'PrimArray'.
peekArray0 :: forall a. (Prim a, Eq a) => a -> Ptr a -> IO (PrimArray a)
peekArray0 term !ptr = lengthArray0 term ptr >>= \size ->
  peekArray size ptr

-- | Write the 'PrimArray' into memory at the given location.
pokeArray :: forall a. Prim a => Ptr a -> PrimArray a -> IO ()
pokeArray !ptr !arr = flip itraversePrimArray_ arr $ \ix atIx ->
  pokeElemOff ptr ix atIx

-- | Write the 'PrimArray' into memory and terminate the elements
--   with a given terminating element.
pokeArray0 :: forall a. Prim a => a -> Ptr a -> PrimArray a -> IO ()
pokeArray0 term !ptr !arr =
  let !sz = sizeofPrimArray arr
   in flip itraversePrimArray_ arr $ \ix atIx ->
     if ix == sz
       then pokeElemOff ptr ix term
       else pokeElemOff ptr ix atIx

-- | Write a 'PrimArray' into a newly allocated, consecutive
-- sequence of primitive values.
newArray :: forall a. Prim a => PrimArray a -> IO (Ptr a)
newArray !arr = do
  ptr <- mallocArray (sizeofPrimArray arr)
  pokeArray ptr arr
  pure ptr

-- | Write a 'PrimArray' into a newly allocated, consecutive
--   sequence of primitive values, where the end is fixed by
--   the given terminating element.
newArray0 :: forall a. Prim a => a -> PrimArray a -> IO (Ptr a)
newArray0 term !arr = do
  ptr <- mallocArray0 (sizeofPrimArray arr)
  pokeArray0 term ptr arr
  pure ptr

-- | Temporarily store a 'PrimArray' in memory.
withArray :: forall a b. Prim a => PrimArray a -> (Ptr a -> IO b) -> IO b
withArray !arr = withArrayLen arr . const

-- | Like 'withArray', but the action is also passed the size
--   of the 'PrimArray'.
withArrayLen :: forall a b. Prim a => PrimArray a -> (Int -> Ptr a -> IO b) -> IO b
withArrayLen !arr f = allocaArray len $ \ptr -> do
  pokeArray ptr arr
  f len ptr
  where
    !len = sizeofPrimArray arr

-- | Like 'withArray', but a terminator indicates where the array ends.
withArray0 :: forall a b. Prim a => a -> PrimArray a -> (Ptr a -> IO b) -> IO b
withArray0 term !arr = withArrayLen0 term arr . const

-- | Like 'withArrayLen', but a terminator indicates where the array ends.
withArrayLen0 :: forall a b. Prim a => a -> PrimArray a -> (Int -> Ptr a -> IO b) -> IO b
withArrayLen0 term !arr f = allocaArray0 len $ \ptr -> do
  pokeArray0 term ptr arr
  f len ptr
  where
    !len = sizeofPrimArray arr

-- | Return the number of elements in an array, excluding
--   the terminator
lengthArray0 :: forall a. (Prim a, Eq a)
  => a -- ^ terminating element
  -> Ptr a
  -> IO Int
lengthArray0 term !ptr = go 0
  where
    go !ix = peekElemOff ptr ix >>= \val ->
      if val == term then pure ix else go (ix + 1)

-- | Advance a pointer into an array by the given number of elements.
advancePtr :: forall a. Prim a => Ptr a -> Int -> Ptr a
advancePtr !ptr !ix = ptr `plusPtr` (ix * sizeOf @a undefined)

-- | Copy the given number of elements from the source array
--   into the destination array; the memory regions /may not/ overlap.
copyArray :: forall a. Prim a
  => Ptr a -- ^ destination array
  -> Ptr a -- ^ source array
  -> Int -- ^ number of elements to copy
  -> IO ()
copyArray !dest !src !size = F.copyBytes dest src (size * sizeOf @a undefined)

-- | Copy the given number of elements from the source array
--   into the destination array; the memory regions /may/ overlap.
moveArray :: forall a. Prim a
  => Ptr a -- ^ destination array
  -> Ptr a -- ^ source array
  -> Int -- ^ number of elements to copy
  -> IO ()
moveArray !dest !src !size = F.moveBytes dest src (size * sizeOf @a undefined)

--------------------------------------------------------------------------------

-- [Section: Foreign.Marshal.Utils]

with :: forall a b. Prim a => a -> (Ptr a -> IO b) -> IO b
with val f = F.with (PrimStorable val) (coerce f :: Ptr (PrimStorable a) -> IO b)

new :: forall a. Prim a => a -> IO (Ptr a)
new val = coerce (F.new (PrimStorable val))

--------------------------------------------------------------------------------
