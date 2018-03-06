{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Binding.OpenGL.BufferObject where

import           Graphics.Binding.OpenGL.Synchro
import           Foreign
import           Graphics.Binding.OpenGL.Utils
import           Graphics.GL.Core45
import           Graphics.Binding.OpenGL.Types
import           Foreign.Resource
import           Data.Typeable
import           Control.Lens

initBuffer :: MonadIO m
                 => BufferSize
                 -> BufferObjectAttribFlags
                 -> Ptr ()
                 -> m BufferName
initBuffer size attrib@BufferObjectAttribFlags {..} ptr = do
  bufo@(BufferObject n) <- genName'
  glNamedBufferStorage n (bufferObjectSizeInternal size) ptr bitF
  return bufo
  where
    bitF = marshalBufferObjectAttribFlags attrib

bufferSubData :: MonadIO m => BufferObject -> BufferSize -> BufferObjectOffset -> Ptr () -> m ()
bufferSubData (BufferObject n) (BufferSize size) (BufferObjectOffset m) = glNamedBufferSubData n m size

mapBuffer :: MonadIO m => BufferObject -> BufferAccess -> m (Ptr ())
mapBuffer (BufferObject n) = glMapNamedBuffer n . marshalBufferAccess

unmapBuffer :: MonadIO m => BufferObject -> m Bool
unmapBuffer (BufferObject n) = fmap unmarshalGLboolean . glUnmapNamedBuffer $ n

mapBufferRange :: MonadIO m => BufferName -> BufferOffset -> BufferSize -> BufferMapFlags -> m (Ptr ())
mapBufferRange (BufferObject obj) offset leng flags
  = glMapNamedBufferRange obj (fromIntegral offset) (fromIntegral leng) (marshalBufferObjectMapFlags flags)

clearBufferSubData :: MonadIO m => BufferObject -> SizedFormat -> BufferObjectOffset -> BufferSize -> SizedFormat -> GLDataType -> Ptr () -> m ()
clearBufferSubData (BufferObject obj) internalForm offset size form typ
  = glClearNamedBufferSubData obj (marshalSizedFormat internalForm) (fromIntegral offset) (fromIntegral size) (marshalSizedFormat form) (marshalGLDataType typ)

copyBufferSubData :: MonadIO m => BufferObject -> BufferObject -> BufferObjectOffset -> BufferObjectOffset -> BufferSize -> m ()
copyBufferSubData (BufferObject readB) (BufferObject writeB) readOff writeOff size
  = glCopyNamedBufferSubData readB writeB (fromIntegral readOff) (fromIntegral writeOff) (fromIntegral size)

bindBufferRange :: MonadIO m => BufferBindPoint -> BufferObjectIndex -> BufferObject -> BufferObjectOffset -> BufferSize -> m ()
bindBufferRange pt (BufferObjectIndex n) (BufferObject m) (BufferObjectOffset l) (BufferSize k) = glBindBufferRange (marshalBufferBindPoint pt) n m l k

bindBufferBase :: MonadIO m => BufferBindPoint -> BufferObjectIndex -> BufferObject -> m ()
bindBufferBase pt (BufferObjectIndex n) (BufferObject m) = glBindBufferBase (marshalBufferBindPoint pt) n m
