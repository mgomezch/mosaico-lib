{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}

module Graphics.Mosaico.División
  ( División((:-:), (:|:), Hoja)
  , Paso(Primero, Segundo)
  , Rectángulo(Rectángulo, color, imagen)
  )
  where

import Graphics.Mosaico.Imagen (Color, Imagen)
import Text.Show               (Show)

data Rectángulo
  = Rectángulo
    { color ∷ Color
    , imagen ∷ Imagen
    }
  deriving Show

data División
  = Hoja Rectángulo
  | División :-: División
  | División :|: División
  deriving Show

data Paso
  = Primero
  | Segundo
  deriving Show
