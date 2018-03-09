{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Binding
  ( module X
  , UniformBlock(..)
  , bindBlock
  , bindFullDynamicUniformBuffer
  ) where

import Graphics.GL.Types as X
  ( GLfloat
  , GLint
  , GLuint
  , GLboolean
  , GLdouble
  )

import Graphics.Binding.OpenGL.Types as X

import Graphics.Binding.OpenGL.BufferObject as X

import Graphics.Binding.OpenGL.Shader as X

import Graphics.Binding.OpenGL.Uniform as X

import Graphics.Binding.OpenGL.Texture as X

import Graphics.Binding.OpenGL.VertexArray as X

import Graphics.Binding.OpenGL.Window as X

import Graphics.Binding.OpenGL.Rendering as X

import Graphics.Binding.OpenGL.Synchro as X

import Graphics.Binding.GLFW.Window as X

------
import Control.Monad.IO.Class
import Data.Typeable



class UniformBlock a where
  bindBlock_ :: Program -> a -> IO ()

bindBlock :: (MonadIO m, UniformBlock a, ProgramLike b) => b -> a -> m ()
bindBlock prg = liftIO . bindBlock_ (toProgram prg)

bindFullDynamicUniformBuffer :: forall a m b. (MonadIO m, GLWritable a) => b -> UniformBufferBindingLocation -> DynamicBuffer a -> m b
bindFullDynamicUniformBuffer b loc (DynamicBuffer name) = bindUniformBuffer loc name 0 (gSize (Proxy :: Proxy a)) >> return b
