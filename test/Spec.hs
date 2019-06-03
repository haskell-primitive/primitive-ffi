{-# language TypeApplications #-}

{-# options_ghc -fno-warn-orphans #-}

module Main (main) where

import Data.Primitive.Foreign
import Data.Primitive.PrimArray
import Data.Primitive.Types (Prim)
import Test.QuickCheck
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do
  quickCheck putInGetOut

putInGetOut :: PrimArray Int -> Bool
putInGetOut arr = unsafePerformIO $ do
  ptr <- newArray arr
  arr' <- peekArray (sizeofPrimArray arr) ptr
  pure (arr == arr')

instance (Arbitrary a, Prim a) => Arbitrary (PrimArray a) where
  arbitrary = do
    l <- choose (5, 25)
    fmap primArrayFromList (vectorOf l arbitrary)
