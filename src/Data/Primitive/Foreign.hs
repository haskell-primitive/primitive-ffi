--------------------------------------------------------------------------------

{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

--------------------------------------------------------------------------------

module Data.Primitive.Foreign
  ( sizeOf, alignment
  , peek, peekElemOff, peekByteOff
  , poke, pokeElemOff, pokeByteOff

  , alloca, F.allocaBytes, F.allocaBytesAligned
  , malloc, F.mallocBytes
  , calloc, F.callocBytes
  , realloc, F.reallocBytes
  , F.free
  ) where

--------------------------------------------------------------------------------

import Data.Coerce (coerce)
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
peekElemOff ptr idx = coerce
  <$> F.peekElemOff @(PrimStorable a) (castPtr ptr) idx

-- | Write a value to a memory area regarded as an array of
--   values of the same kind. The following equality holds:
--
--   > pokeElemOff addr idx x =
--   >   poke (addr `plusPtr` (idx * sizeOf x)) x
pokeElemOff :: forall a. Prim a => Ptr a -> Int -> a -> IO ()
pokeElemOff ptr idx a = F.pokeElemOff (castPtr ptr) idx (PrimStorable a)

-- | Read a value from a memory location given by a base
--   address and offset. The following equality holds:
--
--   > peekByteOff addr off = peek (addr `plusPtr` off)
peekByteOff :: forall a. Prim a => Ptr Void -> Int -> IO a
peekByteOff ptr idx = coerce
  <$> F.peekByteOff @(PrimStorable a) ptr idx

-- | Write a value to a memory location given by a base
--   address and offset. The following equality holds:
--
--   > pokeByteOff addr off x = poke (addr `plusPtr` off) x
pokeByteOff :: forall a. Prim a => Ptr Void -> Int -> a -> IO ()
pokeByteOff ptr idx a = F.pokeByteOff ptr idx (PrimStorable a)

-- | Read a value from the given memory location.
--
--   Note that the peek and poke functions might require properly
--   aligned addresses to function correctly. This is architecture
--   dependent; thus, portable code should ensure that when peeking
--   or poking values of some type @a@, the alignment constraint for
--   @a@, as given by the function 'alignment' is fulfilled.
peek :: forall a. Prim a => Ptr a -> IO a
peek ptr = coerce <$> F.peek (castPtr @a @(PrimStorable a) ptr)

-- | Write the given value to the given memory location. Alignment
--   restrictions might apply; see 'peek'.
poke :: forall a. Prim a => Ptr a -> a -> IO ()
poke ptr a = F.poke (castPtr ptr) (PrimStorable a)

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
realloc ptr = castPtr <$> F.realloc @a @(PrimStorable b) ptr

--------------------------------------------------------------------------------

-- [Section: Foreign.Marshal.Array]

mallocArray :: forall a. Prim a => Int -> IO (Ptr a)
mallocArray idx = castPtr <$> F.mallocArray @(PrimStorable a) idx

mallocArray0 :: forall a. Prim a => Int -> IO (Ptr a)
mallocArray0 idx = castPtr <$> F.mallocArray0 @(PrimStorable a) idx

allocaArray :: forall a b. Prim a => Int -> (Ptr a -> IO b) -> IO b
allocaArray idx f = F.allocaArray idx (coerce f :: Ptr (PrimStorable a) -> IO b)

allocaArray0 :: forall a b. Prim a => Int -> (Ptr a -> IO b) -> IO b
allocaArray0 idx f = F.allocaArray0 idx (coerce f :: Ptr (PrimStorable a) -> IO b)

reallocArray :: forall a. Prim a => Ptr a -> Int -> IO (Ptr a)
reallocArray ptr idx = castPtr <$> F.reallocArray @(PrimStorable a) (castPtr ptr) idx

reallocArray0 :: forall a. Prim a => Ptr a -> Int -> IO (Ptr a)
reallocArray0 ptr idx = castPtr <$> F.reallocArray0 @(PrimStorable a) (castPtr ptr) idx

callocArray :: forall a. Prim a => Int -> IO (Ptr a)
callocArray idx = castPtr <$> F.callocArray @(PrimStorable a) idx

callocArray0 :: forall a. Prim a => Int -> IO (Ptr a)
callocArray0 idx = castPtr <$> F.callocArray0 @(PrimStorable a) idx

--------------------------------------------------------------------------------
