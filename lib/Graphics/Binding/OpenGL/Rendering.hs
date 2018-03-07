{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Binding.OpenGL.Rendering where

import Graphics.GL.Core45
import Graphics.GL.Types
import Graphics.Binding.OpenGL.Utils
import Data.Bits
import Control.Lens

data ClearBuffer = ClearBuffer
  { _clearBufferColor   :: Bool
  , _clearBufferStencil :: Bool
  , _clearBufferDepth   :: Bool
  } deriving (Eq, Ord, Show)

defaultClearBuffer :: ClearBuffer
defaultClearBuffer = ClearBuffer False False False

marshalClearBuffer :: ClearBuffer -> GLbitfield
marshalClearBuffer ClearBuffer {..}
  = cbit .|. sbit .|. dbit
  where
    cbit = if _clearBufferColor then GL_COLOR_BUFFER_BIT else 0
    sbit = if _clearBufferStencil then GL_STENCIL_BUFFER_BIT else 0
    dbit = if _clearBufferDepth then GL_DEPTH_BUFFER_BIT else 0

data PrimitiveMode
  = Points
  | LineStrip
  | LineLoop
  | Lines
  | LineStripAdjacency
  | LinesAdjacency
  | TriangleStrip
  | TriangleFan
  | Triangles
  | TriangleStripAdjacency
  | TrianglesAdjacency
  | Patches
  deriving (Eq, Ord, Show)

marshalPrimitiveMode :: PrimitiveMode -> GLenum
marshalPrimitiveMode = \case
  Points                 -> GL_POINTS
  LineStrip              -> GL_LINE_STRIP
  LineLoop               -> GL_LINE_LOOP
  Lines                  -> GL_LINES
  LineStripAdjacency     -> GL_LINE_STRIP_ADJACENCY
  LinesAdjacency         -> GL_LINES_ADJACENCY
  TriangleStrip          -> GL_TRIANGLE_STRIP
  TriangleFan            -> GL_TRIANGLE_FAN
  Triangles              -> GL_TRIANGLES
  TriangleStripAdjacency -> GL_TRIANGLE_STRIP_ADJACENCY
  TrianglesAdjacency     -> GL_TRIANGLES_ADJACENCY
  Patches                -> GL_PATCHES

data IndexType
  = UnsignedByte
  | UnsignedShort
  | UnsignedInt
  deriving (Eq, Ord, Show)

marshalIndexType :: IndexType -> GLenum
marshalIndexType = \case
  UnsignedByte  -> GL_UNSIGNED_BYTE
  UnsignedShort -> GL_UNSIGNED_SHORT
  UnsignedInt   -> GL_UNSIGNED_INT

clear :: MonadIO m => ClearBuffer -> m ()
clear = liftIO . glClear . marshalClearBuffer

drawElements :: MonadIO m => PrimitiveMode -> GLsizei -> IndexType -> m ()
drawElements mode count typ = glDrawElements (marshalPrimitiveMode mode) count (marshalIndexType typ) nullPtr

makeLenses ''ClearBuffer
