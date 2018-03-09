{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Binding.OpenGL.Uniform where

import Graphics.Binding.OpenGL.Utils
import Graphics.Binding.OpenGL.Shader
import Graphics.Binding.OpenGL.BufferObject
import Graphics.GL.Core45
import Graphics.GL.Types

newtype UniformBufferBindingLocation = UniformBufferBindingLocation
  { _getUniformBufferBindingLocationGLuint :: GLuint
  } deriving (Eq, Ord, Num)

instance Show UniformBufferBindingLocation where
  show (UniformBufferBindingLocation n) = "UniformBufferBindingLocation " `mappend` show n

class DefaultBlockUniform a where
  type DefaultBlockUniformContents a
  defaultBlockUniform_ :: MonadIO m => Program -> a -> DefaultBlockUniformContents a -> m ()

defaultBlockUniform :: (MonadIO m, ProgramLike b, DefaultBlockUniform a) => b -> a -> DefaultBlockUniformContents a -> m ()
defaultBlockUniform b = defaultBlockUniform_ (toProgram b)

bindUniformBuffer :: MonadIO m => UniformBufferBindingLocation -> BufferName -> BufferOffset -> BufferSize -> m ()
bindUniformBuffer (UniformBufferBindingLocation n) (BufferName m) (BufferOffset o) (BufferSize p) = glBindBufferRange GL_UNIFORM_BUFFER n m o p
