{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Graphics.Mosaico.Ventana
  ( Ventana
  , cerrar
  , crearVentana
  , leerTecla
  , mostrar
  )
  where

import Control.Applicative                (pure)
import Control.Concurrent                 (forkIO)
import Control.Concurrent.STM.TMChan      (newTMChanIO, readTMChan, writeTMChan)
import Control.Concurrent.STM.TVar        (newTVarIO, readTVarIO, writeTVar)
import Control.Monad                      (void)
import Control.Monad.STM                  (atomically)
import Control.Monad.Unicode              ((=≪))
import Control.Monad.IO.Class             (liftIO)
import Data.Bool                          (Bool(True))
import Data.Colour.Names                  (blue, green, red, yellow)
import Data.Colour.SRGB                   (Colour, sRGB24)
import Data.Function                      (($), flip)
import Data.Function.Unicode              ((∘))
import Data.Functor                       ((<$>))
import Data.List                          (reverse)
import Data.Maybe                         (Maybe(Nothing, Just))
import Data.Monoid                        (mappend, mempty)
import Data.String                        (String)
import Diagrams.Attributes                (opacity)
import Diagrams.Backend.Cairo             (Cairo)
import Diagrams.Backend.Gtk               (renderToGtk, toGtkCoords)
import Diagrams.BoundingBox               (boundingBox, boxExtents)
import Diagrams.Core.Types                (Diagram)
import Diagrams.TwoD.Align                (centerXY)
import Diagrams.TwoD.Attributes           (fillColor, lineColor, lineWidth, ultraThick)
import Diagrams.TwoD.Combinators          ((===), (|||))
import Diagrams.TwoD.Shapes               (rect, unitSquare)
import Diagrams.TwoD.Size                 (SizeSpec2D(Dims), sized)
import Diagrams.TwoD.Transform            (scaleX, scaleY)
import Diagrams.TwoD.Types                (R2(R2))
import Diagrams.Util                      (( # ))
import Graphics.Mosaico.Imagen            (Imagen(Imagen, altura, anchura), Color(Color, rojo, verde, azul))
import Graphics.Mosaico.División          (División((:-:), (:|:), Hoja), Paso(Primero, Segundo), Rectángulo(Rectángulo, color, imagen))
import Graphics.UI.Gtk.Abstract.Container (containerChild)
import Graphics.UI.Gtk.Abstract.Widget    (EventMask(KeyPressMask), Requisition(Requisition), exposeEvent, keyPressEvent, onDestroy, sizeRequest, widgetAddEvents, widgetDestroy, widgetQueueDraw, widgetShowAll)
import Graphics.UI.Gtk.Gdk.EventM         (eventKeyName, eventWindow)
import Graphics.UI.Gtk.General.General    (initGUI, mainGUI, mainQuit, postGUIAsync, postGUISync)
import Graphics.UI.Gtk.Misc.DrawingArea   (drawingAreaNew)
import Graphics.UI.Gtk.Windows.Window     (windowNew)
import Prelude                            (Double, Integer, fromInteger, fromIntegral)
import System.Glib.Attributes             (AttrOp((:=)), set)
import System.Glib.Signals                (on)
import System.Glib.UTFString              (glibToString)
import System.IO                          (IO)



data Ventana
  = Ventana
    { mostrar   ∷ [Paso] → División → IO ()
    , leerTecla ∷ IO (Maybe String)
    , cerrar    ∷ IO ()
    }



crearVentana ∷ Integer → Integer → IO Ventana
crearVentana anchura' altura'
  = do
    chan      ← newTMChanIO
    diagramaV ← newTVarIO mempty

    void initGUI

    window      ← windowNew
    drawingArea ← drawingAreaNew

    set window [containerChild := drawingArea]

    void
      $ drawingArea `on` sizeRequest
      $ pure (Requisition (fromInteger anchura') (fromInteger altura'))

    void
      $ window `on` keyPressEvent
      $ do
        key ← glibToString <$> eventKeyName
        liftIO
          ∘ void
          ∘ atomically
          $ writeTMChan chan key
        pure True

    void
      $ drawingArea `on` exposeEvent
      $ do
        w ← eventWindow
        liftIO
          $ do
            renderToGtk w
              ∘ toGtkCoords
              ∘ sized (Dims (fromIntegral anchura') (fromIntegral altura'))
              =≪ readTVarIO diagramaV
        pure True

    void $ onDestroy window mainQuit

    widgetAddEvents window [KeyPressMask]
    widgetShowAll window

    void $ forkIO mainGUI

    let
      ventana
        = Ventana {..}

      cerrar
        = postGUISync
        $ widgetDestroy window

      leerTecla
        = atomically
        $ readTMChan chan

      mostrar pasos división
        = postGUIAsync
        $ do
          atomically
            ∘ writeTVar diagramaV
            $ renderDivisión pasos división

          widgetQueueDraw drawingArea

    pure ventana



renderDivisión ∷ [Paso] → División → Diagram Cairo R2
renderDivisión
  = go ∘ pure ∘ reverse
  where
    go pasos
      = centerXY
      ∘ \ case
        d1 :-: d2 → foco blue (go pasosPrimero d1) === foco red (go pasosSegundo d2)
        d1 :|: d2 → foco blue (go pasosPrimero d1) ||| foco red (go pasosSegundo d2)
        Hoja
          Rectángulo { color = Color {..}, imagen = Imagen {..} }
          → foco yellow
          $ unitSquare
          # fillColor (sRGB24 rojo verde azul ∷ Colour Double)
          # scaleX (fromInteger anchura)
          # scaleY (fromInteger altura )
      where
        pasosPrimero
          = case pasos of
            Just (Primero:xs) → Just xs
            _ → Nothing

        pasosSegundo
          = case pasos of
            Just (Segundo:xs) → Just xs
            _ → Nothing

        foco color diagrama
          = case pasos of
            Just []
              → flip mappend (diagrama # centerXY)
              ∘ toRect
              ∘ boxExtents
              $ boundingBox diagrama

            _ → diagrama
          where
            toRect (R2 w h)
              = rect w h
              # fillColor (color ∷ Colour Double)
              # lineColor (green ∷ Colour Double)
              # lineWidth ultraThick
              # opacity 0.25
