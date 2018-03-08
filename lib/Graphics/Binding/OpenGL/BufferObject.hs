{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}

module Graphics.Binding.OpenGL.BufferObject where

import           Graphics.Binding.OpenGL.Synchro
import           Foreign
import           Graphics.Binding.OpenGL.Utils
import           Graphics.GL.Core45
import           Graphics.GL.Types
import           Graphics.Binding.OpenGL.Types
import           Foreign.Resource
import           Data.Typeable
import           Control.Lens

-- * Buffer Objects
newtype BufferName = BufferName
  { getBufferGLuint :: GLuint
  } deriving (Eq, Ord, Show, Storable)

instance ForeignName BufferName () where
  genNames_ = genericGLCreate BufferName glCreateBuffers

  isName_ (BufferName n) = unmarshalGLboolean <$> glIsBuffer n

  deleteNames_ ns = liftIO . withArrayLen ns $ \len ptr -> glDeleteBuffers (fromIntegral len) (castPtr ptr)

newtype BufferSize = BufferSize
  { bufferObjectSizeInternal :: GLsizeiptr
  } deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype BufferOffset = BufferOffset
  { bufferObjectOffsetInternal :: GLintptr
  } deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype BufferRelOffset = BufferRelOffset
  { bufferObjectRelOffsetInternal :: GLuint
  } deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype BufferStride = BufferStride
  { bufferObjectStrideInternal :: GLsizei
  } deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype BufferComponentSize = BufferComponentSize
  { bufferObjectComponentSize :: GLint
  } deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype BufferIndex = BufferIndex
  { bufferObjectIndexInternal :: GLuint
  } deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

data IntegerHandling
  = Normalized
  | NotNormalized
  deriving (Eq, Ord, Show)

data BufferMapType
  = MapNone
  | MapRead
  | MapWrite
  | MapReadWrite
  deriving (Eq, Ord, Show)

data BufferAttribFlags = BufferAttribFlags
  { _bufferAttribFlagsMapType       :: BufferMapType
  , _bufferAttribFlagsMapPersistent :: Bool
  , _bufferAttribFlagsMapCoherent   :: Bool
  , _bufferAttribFlagsDynamic    :: Bool
  , _bufferAttribFlagsClientStorage :: Bool
  } deriving (Eq, Ord, Show)

defaultBufferAttribFlags :: BufferAttribFlags
defaultBufferAttribFlags = BufferAttribFlags
  { _bufferAttribFlagsMapType       = MapNone
  , _bufferAttribFlagsMapPersistent = False
  , _bufferAttribFlagsMapCoherent   = False
  , _bufferAttribFlagsDynamic    = False
  , _bufferAttribFlagsClientStorage = False
  }

marshalBufferAttribFlags :: BufferAttribFlags -> GLbitfield
marshalBufferAttribFlags BufferAttribFlags {..}
  = getBitOr . mconcat . fmap BitOr $
    [bdyna, brewr, bpers, bcohe, bclie]
  where
    brewr = case _bufferAttribFlagsMapType of
      MapNone -> 0
      MapRead -> GL_MAP_READ_BIT
      MapWrite -> GL_MAP_WRITE_BIT
      MapReadWrite -> GL_MAP_READ_BIT .|. GL_MAP_WRITE_BIT
    bpers = if _bufferAttribFlagsMapPersistent then GL_MAP_PERSISTENT_BIT else 0
    bcohe = if _bufferAttribFlagsMapCoherent then GL_MAP_COHERENT_BIT else 0
    bdyna = if _bufferAttribFlagsDynamic then GL_DYNAMIC_STORAGE_BIT else 0
    bclie = if _bufferAttribFlagsClientStorage then GL_CLIENT_STORAGE_BIT else 0

data BufferMapFlags = BufferMapFlags
  { _bufferMapFlagsMapType             :: BufferMapType
  , _bufferMapFlagsMapPersistent       :: Bool
  , _bufferMapFlagsMapCoherent         :: Bool
  , _bufferMapFlagsMapInvalidateRange  :: Bool
  , _bufferMapFlagsMapInvalidateBuffer :: Bool
  , _bufferMapFlagsMapFlushExplicit    :: Bool
  , _bufferMapFlagsMapUnsynchronized   :: Bool
  } deriving (Eq, Ord, Show)

defaultBufferMapFlags :: BufferMapFlags
defaultBufferMapFlags = BufferMapFlags
  { _bufferMapFlagsMapType             = MapNone
  , _bufferMapFlagsMapPersistent       = False
  , _bufferMapFlagsMapCoherent         = False
  , _bufferMapFlagsMapInvalidateRange  = False
  , _bufferMapFlagsMapInvalidateBuffer = False
  , _bufferMapFlagsMapFlushExplicit    = False
  , _bufferMapFlagsMapUnsynchronized   = False
  }

marshalBufferMapFlags :: BufferMapFlags -> GLbitfield
marshalBufferMapFlags BufferMapFlags {..}
  = getBitOr . mconcat . fmap BitOr $
  [brewr, bpers, bcohe, bira, bibu, bfle, bmun]
  where
    brewr = case _bufferMapFlagsMapType of
      MapNone -> 0
      MapRead -> GL_MAP_READ_BIT
      MapWrite -> GL_MAP_WRITE_BIT
      MapReadWrite -> GL_MAP_READ_BIT .|. GL_MAP_WRITE_BIT
    bpers = if _bufferMapFlagsMapPersistent then GL_MAP_PERSISTENT_BIT else 0
    bcohe = if _bufferMapFlagsMapCoherent then GL_MAP_COHERENT_BIT else 0
    bira  = if _bufferMapFlagsMapInvalidateRange then GL_MAP_INVALIDATE_RANGE_BIT else 0
    bibu  = if _bufferMapFlagsMapInvalidateBuffer then GL_MAP_INVALIDATE_BUFFER_BIT else 0
    bfle  = if _bufferMapFlagsMapFlushExplicit then GL_MAP_FLUSH_EXPLICIT_BIT else 0
    bmun  = if _bufferMapFlagsMapUnsynchronized then GL_MAP_UNSYNCHRONIZED_BIT else 0

newtype DynamicBuffer a = DynamicBuffer
  { _getDynamicBufferName :: BufferName
  } deriving (Eq, Ord, Show)

instance GLWritable a => ForeignName (DynamicBuffer a) () where
  genName_ _ = do
    bufo@(BufferName n) <- genName'
    glNamedBufferStorage n (gSize (Proxy :: Proxy a)) nullPtr bitF
    return $ DynamicBuffer bufo
    where
      bitF = GL_DYNAMIC_STORAGE_BIT

  isName_ = isName_ . _getDynamicBufferName

  deleteNames_ = deleteNames_ . fmap _getDynamicBufferName

data FullBufferWrite = FullBufferWrite
  deriving (Eq, Ord, Show)

instance GLWritable a => ForeignWrite FullBufferWrite (DynamicBuffer a) a where
  writeR_ _ b@(DynamicBuffer n) a = allocaBytes size $ \ptr -> do
    gPoke ptr a
    bufferSubData n (fromIntegral size) 0 (castPtr ptr) >> return b
    where
      size = gSize (Proxy :: Proxy a)

------

initBufferName :: MonadIO m
                 => BufferSize
                 -> BufferAttribFlags
                 -> Ptr ()
                 -> m BufferName
initBufferName size attrib@BufferAttribFlags {..} ptr = do
  bufo@(BufferName n) <- genName'
  glNamedBufferStorage n (bufferObjectSizeInternal size) ptr bitF
  return bufo
  where
    bitF = marshalBufferAttribFlags attrib

bufferSubData :: MonadIO m => BufferName -> BufferSize -> BufferOffset -> Ptr () -> m ()
bufferSubData (BufferName n) (BufferSize size) (BufferOffset m) = glNamedBufferSubData n m size

mapBuffer :: MonadIO m => BufferName -> BufferAccess -> m (Ptr ())
mapBuffer (BufferName n) = glMapNamedBuffer n . marshalBufferAccess

unmapBuffer :: MonadIO m => BufferName -> m Bool
unmapBuffer (BufferName n) = fmap unmarshalGLboolean . glUnmapNamedBuffer $ n

mapBufferRange :: MonadIO m => BufferName -> BufferOffset -> BufferSize -> BufferMapFlags -> m (Ptr ())
mapBufferRange (BufferName obj) offset leng flags
  = glMapNamedBufferRange obj (fromIntegral offset) (fromIntegral leng) (marshalBufferMapFlags flags)

clearBufferSubData :: MonadIO m => BufferName -> SizedFormat -> BufferOffset -> BufferSize -> SizedFormat -> GLDataType -> Ptr () -> m ()
clearBufferSubData (BufferName obj) internalForm offset size form typ
  = glClearNamedBufferSubData obj (marshalSizedFormat internalForm) (fromIntegral offset) (fromIntegral size) (marshalSizedFormat form) (marshalGLDataType typ)

copyBufferSubData :: MonadIO m => BufferName -> BufferName -> BufferOffset -> BufferOffset -> BufferSize -> m ()
copyBufferSubData (BufferName readB) (BufferName writeB) readOff writeOff size
  = glCopyNamedBufferSubData readB writeB (fromIntegral readOff) (fromIntegral writeOff) (fromIntegral size)

makeFields ''BufferAttribFlags
makeFields ''BufferMapFlags
