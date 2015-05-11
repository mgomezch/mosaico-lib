{-|
Module      : Graphics.Mosaico.Ventana
Description : Ventanas interactivas con distribuciones de rectángulos
Copyright   : ⓒ Manuel Gómez, 2015
License     : BSD3
Maintainer  : targen@gmail.com
Stability   : experimental
Portability : portable

Representación orientada a objetos de una ventana interactiva donde se puede
mostrar un 'Diagrama' con una parte enfocada, y obtener eventos de teclas
pulsadas en la ventana.
-}

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
import Control.Concurrent.STM.TMChan      (newTMChanIO, closeTMChan, readTMChan, writeTMChan)
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
import Graphics.Mosaico.Diagrama          (Diagrama((:-:), (:|:), Hoja), Paso(Primero, Segundo), Rectángulo(Rectángulo, color, imagen))
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



-- | Un valor del tipo 'Ventana' es un objeto que representa a una ventana
-- interactiva donde puede dibujarse un 'Diagrama'.  Es posible, además,
-- obtener información de qué teclas son pulsadas sobre la ventana.
data Ventana
  = Ventana
    { mostrar   ∷ [Paso] → Diagrama → IO ()
    -- ^ Dada una 'Ventana', un 'Diagrama', y una lista de 'Paso's,
    -- representar gráficamente el 'Diagrama' dado sobre el lienzo de la
    -- 'Ventana', haciendo resaltar visualmente el nodo del árbol alcanzado
    -- si se realizan los movimientos correspondientes a la lista de
    -- 'Paso's desde la raíz del árbol.
    --
    -- Los nodos se resaltan con un cuadro verde, y se colorean según el
    -- tipo de nodo.  En el caso de nodos intermedios, se colorea en azul
    -- la región correspondiente al primer subárbol del nodo binario, y en
    -- rojo la región correspondiente al segundo subárbol.  En el caso de
    -- nodos terminales (hojas), el rectángulo se colorea en amarillo.

    , leerTecla ∷ IO (Maybe String)
    -- ^ Dada una 'Ventana', esperar por un evento de teclado.
    --
    -- Cuando sobre la ventana se haya pulsado alguna tecla que no haya sido
    -- reportada a través de este cómputo, se producirá como resultado
    -- @'Just' tecla@, donde @tecla@ será el nombre de la tecla.
    --
    -- Si la ventana ya ha sido cerrada, se producirá como resultado
    -- 'Nothing'.
    --
    -- El texto correspondiente a cada tecla es aproximadamente igual al
    -- nombre del símbolo en la biblioteca GDK sin el prefijo @GDK_KEY_@.
    -- La lista completa está disponible en
    -- <https://git.gnome.org/browse/gtk+/plain/gdk/gdkkeysyms.h el código fuente de la biblioteca GDK>.
    -- Sin embargo, la mejor manera de descubrir cuál simbolo corresponde
    -- a cada tecla es crear una 'Ventana' y hacer que se imprima el texto
    -- correspondiente a cada tecla pulsada sobre ella.

    , cerrar    ∷ IO ()
    -- ^ Dada una 'Ventana', hacer que se cierre y que no pueda producir
    -- más eventos de teclado.
    }



-- | Construye un objeto del tipo 'Ventana' dadas sus dimensiones en número
-- de píxeles.
crearVentana
  ∷ Integer    -- ^ Número de píxeles de anchura de la 'Ventana' a crear.
  → Integer    -- ^ Número de píxeles de altura de la 'Ventana' a crear.
  → IO Ventana -- ^ La 'Ventana' nueva, ya visible, con el lienzo en blanco.

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

    void
      $ onDestroy window
      $ do
        mainQuit
        atomically $ closeTMChan chan

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

      mostrar pasos diagrama
        = postGUIAsync
        $ do
          atomically
            ∘ writeTVar diagramaV
            $ renderDiagrama pasos diagrama

          widgetQueueDraw drawingArea

    pure ventana



renderDiagrama ∷ [Paso] → Diagrama → Diagram Cairo R2
renderDiagrama
  = go ∘ pure
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
