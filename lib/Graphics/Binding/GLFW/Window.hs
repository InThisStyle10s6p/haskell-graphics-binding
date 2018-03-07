{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Graphics.Binding.GLFW.Window
  ( module X
  , GLContextVersion(..)
  , defaultGraphicsContext
  , WindowConfig(..)
  , CurrentContext(..)
  , WindowShouldClose(..)
  , defaultWindowConfig
  , graphicsContextClientAPI
  , graphicsContextContextVersionMajor
  , graphicsContextContextVersionMinor
  , graphicsContextContextRobustness
  , graphicsContextOpenGLForwardCompat
  , graphicsContextOpenGLDebugContext
  , graphicsContextOpenGLProfile
  , graphicsContextRefreshRate
  , graphicsContextRedBits
  , graphicsContextGreenBits
  , graphicsContextBlueBits
  , graphicsContextAlphaBits
  , graphicsContextDepthBits
  , graphicsContextStencilBits
  , graphicsContextSamples
  , graphicsContextStereo
  , graphicsContextSRGBCapable
  , windowConfigResizable
  , windowConfigVisible
  , windowConfigDecorated
  , windowConfigWidth
  , windowConfigHeight
  , windowConfigTitle
  , windowConfigMonitorFullscreen
  , windowConfigWindowContextShare
  ) where

import Graphics.UI.GLFW as X
  ( Window
  , OpenGLProfile(..)
  , ContextRobustness(..)
  , WindowCloseCallback
  , CursorInputMode(..)
  , WindowHint(..)
  , swapBuffers
  , MouseButton(..)
  , Key(..)
  )

import qualified Graphics.UI.GLFW             as G
import           Control.Monad.IO.Class
import           ClassyPrelude
import           Control.Lens
import           Foreign.Resource

data GLContextVersion = GLContextVersion
  { _versionMajor  :: Int
  , _versionMinor  :: Int
  , _versionRevis  :: Int
  } deriving (Eq, Ord, Show)

instance ForeignRead Window () GLContextVersion where
  readR_ win _ = do
      major <- G.getWindowContextVersionMajor win
      minor <- G.getWindowContextVersionMinor win
      revis <- G.getWindowContextVersionRevision win
      return $ GLContextVersion major minor revis

data WindowShouldClose = WindowShouldClose
  deriving (Eq, Ord, Show)

instance ForeignRead Window WindowShouldClose Bool where
  readR_ win _ = G.windowShouldClose win

instance ForeignWrite Window WindowShouldClose Bool where
  writeR_ win _ = mkWritePassthrough win G.setWindowShouldClose

instance ForeignUpdate Window WindowShouldClose Bool where

instance ForeignWrite Window () (Maybe WindowCloseCallback) where
  writeR_ win _ = mkWritePassthrough win G.setWindowCloseCallback

instance ForeignRead Window () CursorInputMode where
  readR_ win _ = G.getCursorInputMode win

instance ForeignWrite Window () CursorInputMode where
  writeR_ win _ s = G.setCursorInputMode win s >> return win

data GraphicsContextConfig = GraphicsContextConfig
  { _graphicsContextClientAPI           :: G.ClientAPI
  , _graphicsContextContextVersionMajor :: Int
  , _graphicsContextContextVersionMinor :: Int
  , _graphicsContextContextRobustness   :: G.ContextRobustness
  , _graphicsContextOpenGLForwardCompat :: Bool
  , _graphicsContextOpenGLDebugContext  :: Bool
  , _graphicsContextOpenGLProfile       :: G.OpenGLProfile
  , _graphicsContextRefreshRate         :: Maybe Int
  , _graphicsContextRedBits             :: Int
  , _graphicsContextGreenBits           :: Int
  , _graphicsContextBlueBits            :: Int
  , _graphicsContextAlphaBits           :: Int
  , _graphicsContextDepthBits           :: Int
  , _graphicsContextStencilBits         :: Int
  , _graphicsContextSamples             :: Int
  , _graphicsContextStereo              :: Bool
  , _graphicsContextSRGBCapable         :: Bool
  } deriving (Eq, Ord, Show)

defaultGraphicsContext :: GraphicsContextConfig
defaultGraphicsContext = GraphicsContextConfig
  { _graphicsContextClientAPI           = G.ClientAPI'OpenGL
  , _graphicsContextContextVersionMajor = 4
  , _graphicsContextContextVersionMinor = 5
  , _graphicsContextContextRobustness   = G.ContextRobustness'NoRobustness
  , _graphicsContextOpenGLForwardCompat = True
  , _graphicsContextOpenGLDebugContext  = True
  , _graphicsContextOpenGLProfile       = G.OpenGLProfile'Core
  , _graphicsContextRefreshRate         = Nothing
  , _graphicsContextRedBits             = 8
  , _graphicsContextGreenBits           = 8
  , _graphicsContextBlueBits            = 8
  , _graphicsContextAlphaBits           = 8
  , _graphicsContextDepthBits           = 24
  , _graphicsContextStencilBits         = 8
  , _graphicsContextSamples             = 4
  , _graphicsContextStereo              = False
  , _graphicsContextSRGBCapable         = False
  }

data WindowConfig = WindowConfig
  { _windowConfigResizable          :: Bool
  , _windowConfigVisible            :: Bool
  , _windowConfigDecorated          :: Bool
  , _windowConfigWidth              :: Int
  , _windowConfigHeight             :: Int
  , _windowConfigTitle              :: String
  , _windowConfigMonitorFullscreen  :: Maybe G.Monitor
  , _windowConfigWindowContextShare :: Maybe G.Window
  } deriving (Eq, Ord, Show)

defaultWindowConfig :: WindowConfig
defaultWindowConfig = WindowConfig
  { _windowConfigResizable          = True
  , _windowConfigVisible            = True
  , _windowConfigDecorated          = True
  , _windowConfigWidth              = 1920
  , _windowConfigHeight             = 1080
  , _windowConfigTitle              = ""
  , _windowConfigMonitorFullscreen  = Nothing
  , _windowConfigWindowContextShare = Nothing
  }

instance ForeignResource Window WindowConfig where
  resource WindowConfig {..} = mkAcquire acquire_ free_
    where
      acquire_ = do
        Just win <- G.createWindow _windowConfigWidth _windowConfigHeight _windowConfigTitle _windowConfigMonitorFullscreen _windowConfigWindowContextShare
        mapM_ G.windowHint
          [ G.WindowHint'Resizable _windowConfigResizable
          , G.WindowHint'Visible _windowConfigVisible
          , G.WindowHint'Decorated _windowConfigDecorated
          ]
        return win
      free_ = G.destroyWindow

instance ForeignResource () GraphicsContextConfig where
  resource GraphicsContextConfig {..} = mkAcquire acquire_ free_
    where
      acquire_ = do
        _ <- liftIO G.init
        mapM_ G.windowHint
          [ G.WindowHint'ClientAPI _graphicsContextClientAPI
          , G.WindowHint'ContextVersionMajor _graphicsContextContextVersionMajor
          , G.WindowHint'ContextVersionMinor _graphicsContextContextVersionMinor
          , G.WindowHint'ContextRobustness _graphicsContextContextRobustness
          , G.WindowHint'OpenGLForwardCompat _graphicsContextOpenGLForwardCompat
          , G.WindowHint'OpenGLDebugContext _graphicsContextOpenGLDebugContext
          , G.WindowHint'OpenGLProfile _graphicsContextOpenGLProfile
          , G.WindowHint'RedBits _graphicsContextRedBits
          , G.WindowHint'GreenBits _graphicsContextGreenBits
          , G.WindowHint'BlueBits _graphicsContextBlueBits
          , G.WindowHint'AlphaBits _graphicsContextAlphaBits
          , G.WindowHint'DepthBits _graphicsContextDepthBits
          , G.WindowHint'StencilBits _graphicsContextStencilBits
          , G.WindowHint'Samples _graphicsContextSamples
          , G.WindowHint'Stereo _graphicsContextStereo
          , G.WindowHint'sRGBCapable _graphicsContextSRGBCapable
          ]
        forM_ _graphicsContextRefreshRate $ G.windowHint . G.WindowHint'RefreshRate
      free_ _ = G.terminate

data CurrentContext = CurrentContext deriving (Eq, Ord, Show)

instance ForeignRead CurrentContext () (Maybe Window) where
  readR_ _ _ = G.getCurrentContext

instance ForeignWrite CurrentContext () (Maybe Window) where
  writeR_ _ _ mwin = G.makeContextCurrent mwin >> return CurrentContext

mconcat <$> mapM makeLenses
  [ ''WindowConfig
  , ''GraphicsContextConfig
  ]

