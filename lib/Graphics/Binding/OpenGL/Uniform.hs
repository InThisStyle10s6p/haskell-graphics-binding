{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Binding.OpenGL.Uniform where

import Graphics.GL.Core45
import Graphics.GL.Types
import Graphics.Binding.OpenGL.Utils
import Graphics.Binding.OpenGL.Shader
import Graphics.Binding.OpenGL.BufferObject
import Graphics.Binding.OpenGL.VertexArray

newtype UniformBufferBindingLocation = UniformBufferBindingLocation
  { _getUniformBufferBindingLocationGLuint :: GLuint
  } deriving (Eq, Ord, Show, Num)

class DefaultBlockUniform a where
  type DefaultBlockUniformContents a
  defaultBlockUniform_ :: MonadIO m => Program -> a -> DefaultBlockUniformContents a -> m ()

defaultBlockUniform :: (MonadIO m, ProgramLike b, DefaultBlockUniform a) => b -> a -> DefaultBlockUniformContents a -> m ()
defaultBlockUniform b = defaultBlockUniform_ (toProgram b)

uniformBlockBinding :: MonadIO m => Program -> UniformBlockLocation -> UniformBufferBindingLocation -> m ()
uniformBlockBinding (Program a) (UniformBlockLocation b) (UniformBufferBindingLocation c) = glUniformBlockBinding a b c

bindUniformBuffer :: MonadIO m => UniformBufferBindingLocation -> BufferName -> BufferOffset -> BufferSize -> m ()
bindUniformBuffer (UniformBufferBindingLocation n) (BufferName m) (BufferOffset o) (BufferSize p) = glBindBufferRange GL_UNIFORM_BUFFER n m o p
