{-|
Module      : Graphics.Mosaico.División
Description : Distribuciones espaciales de rectángulos coloridos
Copyright   : ⓒ Manuel Gómez, 2015
License     : BSD3
Maintainer  : targen@gmail.com
Stability   : experimental
Portability : portable

Tipos útiles para representar distribuciones espaciales en dos dimensiones de
rectángulos de colores calculados a partir de imágenes.
-}

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



-- | Un @Rectángulo@ representa a una región rectangular coloreada
-- uniformemente con su 'color', que ha sido calculado a partir de su
-- 'imagen'.  El tamaño del rectángulo es el mismo de su imagen.
data Rectángulo
  = Rectángulo
    { color ∷ Color   -- ^ El 'Color' del rectángulo.
    , imagen ∷ Imagen -- ^ La 'Imagen' usada para calcular el color del
                      -- rectángulo.
    }
  deriving Show



-- | Una @División@ es un árbol binario decorado en las hojas con
-- @Rectángulo@s, y representa una distribución espacial de @Rectángulo@s
-- en dos dimensiones.  Hay dos especies de nodos intermedios que se usan
-- para distribuir los rectángulos de los subárboles de izquierda
-- a derecha, o arriba y abajo.
data División

  -- | El constructor para las hojas del árbol que contienen un
  -- @Rectángulo@.
  = Hoja Rectángulo

  -- | Este constructor se usa para especificar que la distribución
  -- espacial de rectángulos del primer subárbol se ubica arriba de la
  -- distribución de rectángulos del segundo subárbol.
  | División :-: División

  -- | Este constructor se usa para especificar que la distribución
  -- espacial de rectángulos del primer subárbol se ubica a la la izquierda
  -- de la distribución de rectángulos del segundo subárbol.
  | División :|: División
  deriving Show



-- | Un tipo sencillo conveniente para indicar pasos de caminos en una
-- @División@.
data Paso

  -- | Un paso hacia el primer subárbol en un nodo intermedio.
  = Primero

  -- | Un paso hacia el segundo subárbol en un nodo intermedio.
  | Segundo
  deriving Show
