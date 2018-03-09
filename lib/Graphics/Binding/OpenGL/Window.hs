{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Graphics.Binding.OpenGL.Window where

import Data.ByteString
import Foreign.Ptr
import Foreign.Resource
import Graphics.GL.Types
import Graphics.GL.Core45
import Linear
import Text.Printf

-- * Window
data Face = Front | Back | FrontBack deriving (Eq, Ord, Show)

newtype Color4 = Color4 (V4 GLfloat)

data DepthFunc
  = DepthNever
  | DepthLess
  | DepthEqual
  | DepthLEqual
  | DepthGreater
  | DepthNotEqual
  | DepthGEqual
  | DepthAlways
  deriving (Eq, Ord, Show)

data DebugSource
  = DebugSourceAPI
  | DebugSourceWindowSystem
  | DebugSourceShaderCompiler
  | DebugSourceThirdParty
  | DebugSourceApplication
  | DebugSourceOther
  deriving (Eq, Ord, Show)

data DebugType
  = DebugTypeError
  | DebugTypeDeprecatedBehavior
  | DebugTypeUndefinedBehavior
  | DebugTypePortability
  | DebugTypePerformance
  | DebugTypeMarker
  | DebugTypePushGroup
  | DebugTypePopGroup
  | DebugTypeOther
  deriving (Eq, Ord, Show)

data DebugSeverity
  = DebugSeverityHigh
  | DebugSeverityMedium
  | DebugSeverityLow
  | DebugSeverityNotification
  deriving (Eq, Ord, Show)

newtype DebugID = DebugID GLuint deriving (Eq, Ord, Show)

type DebugCallbackFun = DebugSource -> DebugType -> DebugID -> DebugSeverity -> ByteString -> IO ()

unmarshalDebugSource :: GLenum -> DebugSource
unmarshalDebugSource = \case
  GL_DEBUG_SOURCE_API             -> DebugSourceAPI
  GL_DEBUG_SOURCE_WINDOW_SYSTEM   -> DebugSourceWindowSystem
  GL_DEBUG_SOURCE_SHADER_COMPILER -> DebugSourceShaderCompiler
  GL_DEBUG_SOURCE_THIRD_PARTY     -> DebugSourceThirdParty
  GL_DEBUG_SOURCE_APPLICATION     -> DebugSourceApplication
  GL_DEBUG_SOURCE_OTHER           -> DebugSourceOther
  _                            -> error "Unrecognized debug source.\n"

unmarshalDebugType   :: GLenum -> DebugType
unmarshalDebugType = \case
  GL_DEBUG_TYPE_ERROR               -> DebugTypeError
  GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR -> DebugTypeDeprecatedBehavior
  GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR  -> DebugTypeUndefinedBehavior
  GL_DEBUG_TYPE_PORTABILITY         -> DebugTypePortability
  GL_DEBUG_TYPE_PERFORMANCE         -> DebugTypePerformance
  GL_DEBUG_TYPE_MARKER              -> DebugTypeMarker
  GL_DEBUG_TYPE_PUSH_GROUP          -> DebugTypePushGroup
  GL_DEBUG_TYPE_POP_GROUP           -> DebugTypePopGroup
  GL_DEBUG_TYPE_OTHER               -> DebugTypeOther
  _                              -> error "Unrecognized debug type.\n"

unmarshalDebugSeverity :: GLenum -> DebugSeverity
unmarshalDebugSeverity = \case
  GL_DEBUG_SEVERITY_HIGH         -> DebugSeverityHigh
  GL_DEBUG_SEVERITY_MEDIUM       -> DebugSeverityMedium
  GL_DEBUG_SEVERITY_LOW          -> DebugSeverityLow
  GL_DEBUG_SEVERITY_NOTIFICATION -> DebugSeverityNotification
  _                           -> error "Unrecognized debug severity.\n"


data FaceCullMode = FaceCullMode
  deriving (Eq, Ord, Show)

instance ForeignWrite () FaceCullMode (Maybe Face) where
  writeR_ _ _ = \case
    Nothing        -> glDisable GL_CULL_FACE >> return FaceCullMode
    Just Front     -> glEnable GL_CULL_FACE >> glCullFace GL_FRONT >> return FaceCullMode
    Just Back      -> glEnable GL_CULL_FACE >> glCullFace GL_BACK >> return FaceCullMode
    Just FrontBack -> glEnable GL_CULL_FACE >> glCullFace GL_FRONT_AND_BACK >> return FaceCullMode

color4 :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Color4
color4 r g b = Color4 . V4 r g b

data ClearColor = ClearColor
  deriving (Eq, Ord, Show)

instance ForeignWrite () ClearColor Color4 where
  writeR_ _ _ (Color4 (V4 r g b a)) = glClearColor r g b a >> return ClearColor

data DepthTest = DepthTest
  deriving (Eq, Ord, Show)

instance ForeignWrite () DepthTest (Maybe DepthFunc) where
  writeR_ _ _ = \case
    Nothing -> glDisable GL_DEPTH_TEST >> return DepthTest
    Just DepthNever     -> glEnable GL_DEPTH_TEST >> glDepthFunc GL_NEVER >> return DepthTest
    Just DepthLess      -> glEnable GL_DEPTH_TEST >> glDepthFunc GL_LESS >> return DepthTest
    Just DepthEqual     -> glEnable GL_DEPTH_TEST >> glDepthFunc GL_EQUAL >> return DepthTest
    Just DepthLEqual    -> glEnable GL_DEPTH_TEST >> glDepthFunc GL_LEQUAL >> return DepthTest
    Just DepthGreater   -> glEnable GL_DEPTH_TEST >> glDepthFunc GL_GREATER >> return DepthTest
    Just DepthNotEqual  -> glEnable GL_DEPTH_TEST >> glDepthFunc GL_NOTEQUAL >> return DepthTest
    Just DepthGEqual    -> glEnable GL_DEPTH_TEST >> glDepthFunc GL_GEQUAL >> return DepthTest
    Just DepthAlways    -> glEnable GL_DEPTH_TEST >> glDepthFunc GL_ALWAYS >> return DepthTest

data DebugMessageCallback = DebugMessageCallback
  deriving (Eq, Ord, Show)

instance ForeignWrite () DebugMessageCallback (Maybe DebugCallbackFun) where
  writeR_ _ _ = \case
    Nothing -> glDebugMessageCallback nullFunPtr nullPtr >> return DebugMessageCallback
    Just func -> do
      fp <- mkGLDEBUGPROC func'
      glDebugMessageCallback fp nullPtr >> return DebugMessageCallback
        where
          func' src typ ident sever len msg _ = do
            msg' <- packCStringLen (msg, fromIntegral len)
            func src' typ' id' sev' msg'
              where
                src' = unmarshalDebugSource src
                typ' = unmarshalDebugType typ
                id'  = DebugID ident
                sev' = unmarshalDebugSeverity sever

simpleDebugFunc :: DebugCallbackFun
simpleDebugFunc src typ ident sever msg = printf "!!! OpenGL Error. %s; %s; %s; %s;\n Message: %s\n\n" (show sever) (show ident) (show src) (show typ) (show msg)
