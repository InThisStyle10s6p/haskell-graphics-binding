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

import Graphics.Binding.OpenGL.Program as X

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
{-
fencePersistentBuffer :: MonadIO m => PersistentBuffer a -> m (PersistentBuffer a)
fencePersistentBuffer pb = do
  sync <- lockGLFence (_getPersistentBufferSync pb)
  return pb { _getPersistentBufferSync = sync}

instance GLWritable a => ForeignName (PersistentBuffer a) () where
  genName_ _ = do
    bufo <- genName (size, persistentBufferObjectFlag)
    ptr <- mapSizedBufferRange_ bufo 0 size persistentBufferMapFlag
    return $ PersistentBuffer (castPtr ptr) bufo nullPtr
      where
        size = fromIntegral $ gSize (Proxy :: Proxy a)

  isName_ (PersistentBuffer _ bufo _) = isName_ bufo

  deleteName_ (PersistentBuffer bufPtr bufo lock) = do
    deleteFence lock
    _ <- unmapSizedBuffer bufo
    deleteName_ bufo

data PersistentBuffer a = PersistentBuffer
  { _getPersistentBufferPtr    :: Ptr a
  , _getPersistentBufferName   :: BufferName
  , _getPersistentBufferSync   :: GLsync
  } deriving (Eq, Ord, Show)

persistentBufferMapFlag :: BufferMapFlags
persistentBufferMapFlag = BufferMapFlags MapWrite True True False False False False

persistentBufferAttribFlag :: BufferAttribFlags
persistentBufferAttribFlag = BufferAttribFlags MapWrite True True False False

class BufferObject a where
  bindUniformBuffer_ :: UniformBufferBindingLocation -> a -> BufferOffset -> BufferSize -> IO ()

bindUniformBuffer :: (MonadIO m, BufferObject a) => UniformBufferBindingLocation -> a -> BufferOffset -> BufferSize -> m ()
bindUniformBuffer loc a off size = liftIO $ bindUniformBuffer_ loc a off size
-}

class UniformBlock a where
  bindBlock_ :: Program -> a -> IO ()

bindBlock :: (MonadIO m, UniformBlock a) => Program -> a -> m ()
bindBlock prg = liftIO . bindBlock_ prg

bindFullDynamicUniformBuffer :: forall a m b. (MonadIO m, GLWritable a) => b -> UniformBufferBindingLocation -> DynamicBuffer a -> m b
bindFullDynamicUniformBuffer b loc (DynamicBuffer name) = bindUniformBuffer loc name 0 (gSize (Proxy :: Proxy a)) >> return b
