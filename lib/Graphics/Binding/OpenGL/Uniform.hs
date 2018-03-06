{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Binding.OpenGL.Uniform where

import Graphics.GL.Core45
import Graphics.Binding.OpenGL.Utils
import Graphics.Binding.OpenGL.Types
import Graphics.Binding.OpenGL.BufferObject
import Data.Typeable

uniformBlockBinding :: MonadIO m => Program -> UniformBlockLocation -> UniformBufferBindingLocation -> m ()
uniformBlockBinding (Program a) (UniformBlockLocation b) (UniformBufferBindingLocation c) = glUniformBlockBinding a b c

bindUniformBuffer :: MonadIO m => UniformBufferBindingLocation -> BufferName -> BufferObjectOffset -> BufferObjectSize -> m()

bindUniformBuffer (UniformBufferBindingLocation n) (BufferName m) (BufferObjectOffset o) (BufferObjectSize p) = glBindBufferRange n m o p
