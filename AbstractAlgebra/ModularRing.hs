{- |
Module      :  $Header$
Description :  Module handling modular rings
Copyright   :  (c) Michal Parusinski
License     :  GPLv3

Maintainer  :  mparusinski@gmail.com
Stability   :  experimental
Portability :  portable

<module description starting at first column>
-}

module AbstractAlgebra.ModularRings where

import AbstractAlgebra.Rings

data (Ring a) => ModularRing a = Modular { representant  :: a ,
                                           ringEmbedding :: a -> a }


instance (Ring a, Show a) => Show (ModularRing a) where
    show (Modular x e) = show x


-- can't compare embeddings but equality is useful
instance (Ring a) => Eq (ModularRing a) where
    Modular x e == Modular y f = x == y

{-
  We can't really represent a modular ring as a ring in Haskell
  as we would require the type parameter to be a function
  So we will define our own "Modular Ring class"
  We assume  an embedding 
-}


embed :: (Ring a) => a -> (a -> a) -> ModularRing a
embed elem embedding 
    = Modular (embedding elem) embedding

mod_zero :: (Ring a) => (a -> a) -> ModularRing a 
mod_zero embedding = Modular zero embedding


mod_one :: (Ring a) => (a -> a) -> ModularRing a
mod_one embedding  = Modular one embedding


mod_lift_op op m_x m_y
    = Modular (em (op (representant m_x) (representant m_y))) em
    where em = ringEmbedding m_x


mod_add :: (Ring a) => ModularRing a -> ModularRing a -> ModularRing a
mod_add mod_x mod_y 
    = mod_lift_op add mod_x mod_y


mod_mult :: (Ring a) => ModularRing a -> ModularRing a -> ModularRing a
mod_mult mod_x mod_y
    = mod_lift_op mult mod_x mod_y


mod_sub :: (Ring a) => ModularRing a -> ModularRing a -> ModularRing a
mod_sub mod_x mod_y
    = mod_lift_op sub mod_x mod_y

mod_neg :: (Ring a) => ModularRing a -> ModularRing a
mod_neg mod_x
    = Modular mod_neg_x em
    where mod_neg_x = em (neg (representant mod_x))
          em        = ringEmbedding mod_x

mod_pow :: (Ring a, Integral b) => ModularRing a -> b -> ModularRing a
mod_pow x 0 = mod_one $ ringEmbedding x
mod_pow mantissa 1 = mantissa
mod_pow mantissa exponent
    | rem == 0  = mod_mult term term
    | otherwise = mod_mult (mod_mult term term) mantissa
    where term        = mod_pow mantissa half
          (half, rem) = divMod exponent 2