{-|
Module      : Graphics.Mosaico.Imagen
Description : Lectura de imágenes como matrices de píxeles
Copyright   : ⓒ Manuel Gómez, 2015
License     : BSD3
Maintainer  : targen@gmail.com
Stability   : experimental
Portability : portable

Tipos para representar imágenes como matrices (listas anidadas rectangulares) de
píxeles de colores, y una función para cargar una imagen a esta representación a
partir de un archivo.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Graphics.Mosaico.Imagen
  ( Color(Color, rojo, verde, azul)
  , Imagen(Imagen, altura, anchura, datos)
  , leerImagen
  )
  where

import Codec.Picture         (readImage)
import Codec.Picture.Types   (DynamicImage(ImageRGB8, ImageYCbCr8), PixelRGB8(PixelRGB8), convertImage, imageWidth, imageHeight, pixelFold)
import Control.Applicative   (pure)
import Control.Monad.Error   (throwError)
import Control.Monad.Unicode ((=≪))
import Data.Either           (Either)
import Data.Function         (($))
import Data.Function.Unicode ((∘))
import Data.Functor          ((<$>))
import Data.List             (reverse)
import Data.List.Split       (chunksOf)
import Data.List.Unicode     ((⧺))
import Data.String           (String)
import Data.Word             (Word8)
import Prelude               (Integer, fromIntegral)
import System.IO             (IO)
import Text.Show             (Show, show)



-- | Un punto en el espacio de colores RGB donde cada componente de color
-- se especifica por un entero entre 0 y 255 (8 bits).  Como el color es
-- toda la información almacenada en un píxel, este mismo tipo se usa para
-- representar píxeles individuales en una 'Imagen'.
data Color
  = Color
    { rojo, verde, azul ∷ Word8
    }
  deriving Show



-- | La representación de una imagen como una matriz de píxeles dados por
-- el 'Color' de cada uno.
--
-- Las dimensiones de la imagen se guardan por separado para no tener que
-- recorrer las listas de píxeles cada vez que haga falta conocer sus
-- longitudes.
data Imagen
  = Imagen
    { anchura ∷ Integer
    -- ^ El número de píxeles en cada fila de la 'Imagen'.

    , altura ∷ Integer
    -- ^ El número de filas de la 'Imagen'.

    , datos ∷ [[Color]]
    -- ^ Los datos de color de cada píxel de la imagen.
    --
    -- Cada píxel se representa con un valor del tipo 'Color' que especifica el
    -- color del píxel.  Los datos se organizan en una lista de filas de la
    -- imagen, donde cada fila es a su vez una lista de cada píxel individual de
    -- esa fila.
    --
    -- Esta lista de listas debe mantenerse rectangular: todas las filas deben
    -- ser listas con la misma longitud, que además debe ser igual a la
    -- 'anchura' de la misma 'Imagen'.  Además, la 'altura' debe ser igual a la
    -- longitud de la lista de filas.
    }

instance Show Imagen where
  show Imagen {..}
    = "Imagen {"
    ⧺ " anchura = " ⧺ show anchura ⧺ ";"
    ⧺ " altura = "  ⧺ show altura  ⧺ ";"
    ⧺ " datos = […];"
    ⧺ " }"



-- | Leer un archivo e intentar convertirlo en una 'Imagen'.  Se soportan
-- varios formatos de archivo de imagen, incluyendo PNG y JPEG.
leerImagen
  ∷ String
  -- ^ El nombre del archivo a leer.

  → IO (Either String Imagen)
  -- ^ Si el archivo pudo leerse exitosamente y representarse como un valor
  -- del tipo 'Imagen', se produce el resultado @'Data.Either.Right' imagen@.
  -- Si no, se produce el resultado @'Data.Either.Left' razón@, donde @razón@
  -- será un 'String' con la razón por la cual la imagen no se pudo leer.

leerImagen filename
  = do
    imagen ← (toImageRGB8 =≪) <$> readImage filename
    pure $ toImagen <$> imagen
  where
    toImageRGB8
      = \ case
        ImageRGB8   i → pure i
        ImageYCbCr8 i → pure $ convertImage i
        -- TODO: more conversions!
        _ → throwError "conversión del formato de imagen a RGB8 no implantada"

    toImagen image
      = Imagen {..}
      where
        anchura = fromIntegral $ imageWidth  image
        altura  = fromIntegral $ imageHeight image
        datos
          = chunksOf (imageWidth image)
          ∘ reverse
          $ pixelFold f [] image
          where
            f acc _ _ (PixelRGB8 rojo verde azul)
              = Color {..} : acc
