{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Matrix where

import Graphics.Rendering.OpenGL

class V v where
  (^+) :: v -> v -> v
  (^*) :: GLfloat -> v -> v
  zero :: v
  toList :: v -> [GLfloat]
infixl 5 ^+
infixl 6 ^*

class (V t) => Measurable t where
  pythagorean :: t -> GLfloat

class M m v | m -> v where
  (*^) :: m -> v -> v
  (^*^) :: m -> m -> m
  identity :: m
infixl 7 *^
infixl 5 ^*^
instance V GLfloat where
  x ^+ y = x + y
  x ^* y = x * y
  zero = 0
  toList x = [x]

data V3 a = V3 a a a deriving (Eq, Show)
type V3f = V3 GLfloat
type M3f = V3 V3f
-- This takes care of vectors and matrices at once
instance (V t) => V (V3 t) where
  V3 x1 x2 x3 ^+ V3 y1 y2 y3 = V3 (x1 ^+ y1) (x2 ^+ y2) (x3 ^+ y3)
  s ^* V3 x1 x2 x3 = V3 (s ^* x1) (s ^* x2) (s ^* x3)
  zero = V3 zero zero zero
  toList (V3 x1 x2 x3) = concatMap toList [x1, x2, x3]
instance Measurable V3f where
  pythagorean (V3 x1 x2 x3) = sqrt (x1*x1 + x2*x2 + x3*x3)
instance M M3f V3f where
  V3 x y z *^ V3 a b c = (a ^* x) ^+ (b ^* y) ^+ (c ^* z)
  v ^*^ V3 d e f = V3 (v *^ d) (v *^ e) (v *^ f)
  identity = V3 (V3 1 0 0)
                (V3 0 1 0)
                (V3 0 0 1)

data V4 a = V4 a a a a deriving (Eq, Show)
type V4f = V4 GLfloat
type M4f = V4 V4f
instance (V t) => V (V4 t) where
  V4 x1 x2 x3 x4 ^+ V4 y1 y2 y3 y4 = V4 (x1 ^+ y1) (x2 ^+ y2) (x3 ^+ y3) (x4 ^+ y4)
  s ^* V4 x1 x2 x3 x4 = V4 (s ^* x1) (s ^* x2) (s ^* x3) (s ^* x4)
  zero = V4 zero zero zero zero
  toList (V4 x1 x2 x3 x4) = concatMap toList [x1, x2, x3, x4]
instance Measurable V4f where
  pythagorean (V4 x1 x2 x3 x4) = sqrt (x1*x1 + x2*x2 + x3*x3 + x4*x4)
instance M M4f V4f where
  V4 x y z w *^ V4 a b c d = (a ^* x) ^+ (b ^* y) ^+ (c ^* z) ^+ (d ^* w)
  v ^*^ V4 d e f g = V4 (v *^ d) (v *^ e) (v *^ f) (v *^ g)
  identity = V4 (V4 1 0 0 0)
                (V4 0 1 0 0)
                (V4 0 0 1 0)
                (V4 0 0 0 1)

normalize v | v == zero = zero
normalize v = (1 / pythagorean v) ^* v


v3 :: GLfloat -> GLfloat -> GLfloat -> V3f
v3 = V3
v4 :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> V4f
v4 = V4
m3 :: V3f -> V3f -> V3f -> M3f
m3 = V3
m4 :: V4f -> V4f -> V4f -> V4f -> M4f
m4 = V4

v4tov3 (V4 a b c 1) = V3 a b c
v4tov3 v = error $ "Vector is not 3-vector: " ++ show v

translationMatrix :: V3f -> M4f
translationMatrix (V3 x y z) =
  m4 (v4 1 0 0 0)
     (v4 0 1 0 0)
     (v4 0 0 1 0)
     (v4 x y z 1)


calculateProjectionMatrix :: Size -> M4f
calculateProjectionMatrix (Size x y) =
  let wf = fromIntegral x
      hf = fromIntegral y
      projectionFovRatio = 0.7
      projectionNearPlane = 0.0625
      projectionFarPlane = 256.0
      r_xy_factor = min wf hf * 1.0 / projectionFovRatio
      r_x = r_xy_factor / wf
      r_y = r_xy_factor / hf
      r_zw_factor = 1.0 / (projectionFarPlane - projectionNearPlane)
      r_z = (projectionNearPlane + projectionFarPlane) * r_zw_factor
      r_w = -2.0 * projectionNearPlane * projectionFarPlane * r_zw_factor
  in m4 (v4 r_x 0 0 0)
        (v4 0 r_y 0 0)
        (v4 0 0 r_z 1)
        (v4 0 0 r_w 0)

calculateModelViewMatrix :: V3f -> M4f
calculateModelViewMatrix v = translationMatrix (-1 ^* v)