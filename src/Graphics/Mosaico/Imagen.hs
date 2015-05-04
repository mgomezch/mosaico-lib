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



data Color
  = Color
    { rojo, verde, azul ∷ Word8
    }
  deriving Show



data Imagen
  = Imagen
    { anchura, altura ∷ Integer
    , datos ∷ [[Color]]
    }

instance Show Imagen where
  show Imagen {..}
    = "Imagen {"
    ⧺ " anchura = " ⧺ show anchura ⧺ ";"
    ⧺ " altura = "  ⧺ show altura  ⧺ ";"
    ⧺ " datos = […];"
    ⧺ " }"



leerImagen ∷ String → IO (Either String Imagen)
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
        _ → throwError "conversion to RGB8 not implemented"

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
