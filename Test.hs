{-# LANGUAGE FlexibleInstances #-}
module Main where

import Matrix

import Test.QuickCheck
import Control.Monad
import Graphics.Rendering.OpenGL (GLfloat)

instance Arbitrary GLfloat where
  arbitrary = fmap (fromRational . toRational) (arbitrary :: Gen Float)
instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = do
    x1 <- arbitrary
    x2 <- arbitrary
    x3 <- arbitrary
    return $ V3 x1 x2 x3
instance Arbitrary a => Arbitrary (V4 a) where
  arbitrary = do
    x1 <- arbitrary
    x2 <- arbitrary
    x3 <- arbitrary
    x4 <- arbitrary
    return $ V4 x1 x2 x3 x4


main :: IO ()
main = do
  testMatrix

testMatrix :: IO ()
testMatrix = do
  testV3

class FuzzyEq a where
  (~==) :: a -> a -> Bool
infixl 1 ~==
instance FuzzyEq GLfloat where
  a ~== b = abs (a-b) / (a*a + b*b + 0.000001) < 0.01
instance (FuzzyEq t) => FuzzyEq (V3 t) where
  V3 a b c ~== V3 d e f = and [a ~== d, b ~== e, c ~== f]
instance (FuzzyEq t) => FuzzyEq (V4 t) where
  V4 a b c d ~== V4 e f g h = and [a ~== e, b ~== f, c ~== g, d ~== h]

testV3 = do
  quickCheck $ \v -> 0 ^* v == (zero :: V3f)
  quickCheck $ \a b v -> (a * b) ^* v ~== a ^* (b ^* v :: V3f)
  quickCheck $ \v -> v ^+ zero == (v :: V3f)
  quickCheck $ \u v -> u ^+ v == (v ^+ u :: V3f)
  quickCheck $ \a u v -> a^*u ^+ a^*v ~== (a^*(u ^+ v) :: V3f)

  quickCheck $ normalize zero == (zero :: V3f)
  quickCheck $ \v -> v /= (zero :: V3f) ==> (pythagorean . normalize) v ~== 1
  
  quickCheck $ \m -> 0 ^* m == (zero :: M3f)
  quickCheck $ \a b m -> (a * b) ^* m ~== a ^* (b ^* m :: M3f)
  quickCheck $ \m -> m ^+ zero == (m :: M3f)
  quickCheck $ \m1 m2 -> m1 ^+ m2 == (m2 ^+ m1 :: M3f)
  quickCheck $ \a m1 m2 -> a^*m1 ^+ a^*m2 ~== (a^*(m1 ^+ m2) :: M3f)
  
  quickCheck $ \m -> (m :: M3f) *^ zero ~== zero
  
  quickCheck $ \m1 m2 v -> (m1 :: M3f) *^ (m2 *^ v) ~== (m1 ^*^ m2) *^ v

testV4 = do
  quickCheck $ \v -> 0 ^* v == (zero :: V4f)
  quickCheck $ \a b v -> (a * b) ^* v ~== a ^* (b ^* v :: V4f)
  quickCheck $ \v -> v ^+ zero == (v :: V4f)
  quickCheck $ \u v -> u ^+ v == (v ^+ u :: V4f)
  quickCheck $ \a u v -> a^*u ^+ a^*v ~== (a^*(u ^+ v) :: V4f)

  quickCheck $ normalize zero == (zero :: V4f)
  quickCheck $ \v -> v /= (zero :: V4f) ==> (pythagorean . normalize) v ~== 1
  
  quickCheck $ \m -> 0 ^* m == (zero :: M4f)
  quickCheck $ \a b m -> (a * b) ^* m ~== a ^* (b ^* m :: M4f)
  quickCheck $ \m -> m ^+ zero == (m :: M4f)
  quickCheck $ \m1 m2 -> m1 ^+ m2 == (m2 ^+ m1 :: M4f)
  quickCheck $ \a m1 m2 -> a^*m1 ^+ a^*m2 ~== (a^*(m1 ^+ m2) :: M4f)
  
  quickCheck $ \m -> (m :: M4f) *^ zero ~== zero
  
  quickCheck $ \m1 m2 v -> (m1 :: M4f) *^ (m2 *^ v) ~== (m1 ^*^ m2) *^ v

  quickCheck $ \v -> (identity :: M4f) *^ v ~== v
  quickCheck $ \v a b c -> toList (translationMatrix v *^ v4 a b c 1) !! 3 == 1