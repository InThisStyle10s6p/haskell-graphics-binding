{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}

module Graphics.Binding.OpenGL.BufferObject where

import Control.Lens
import Foreign
import Foreign.Resource
import Graphics.Binding.OpenGL.Types
import Graphics.Binding.OpenGL.Utils
import Graphics.GL.Core45
import Graphics.GL.Types

-- * Buffer Objects
newtype BufferName = BufferName
  { getBufferGLuint :: GLuint
  } deriving (Eq, Ord, Storable)

instance Show BufferName where
  show (BufferName n) = "BufferName " `mappend` show n

instance ForeignName BufferName () where
  genNames_ = genericGLCreate BufferName glCreateBuffers

  isName_ = genericGLIsName getBufferGLuint glIsBuffer

  deleteNames_ = genericGLDeleteNames getBufferGLuint glDeleteBuffers

newtype BufferSize = BufferSize
  { bufferSizeInternal :: GLsizeiptr
  } deriving (Eq, Ord, Num, Real, Enum, Integral)

instance Show BufferSize where
  show (BufferSize n) = "BufferSize " `mappend` show n

newtype BufferOffset = BufferOffset
  { bufferOffsetInternal :: GLintptr
  } deriving (Eq, Ord, Num, Real, Enum, Integral)

instance Show BufferOffset where
  show (BufferOffset n) = "BufferOffset" `mappend` show n

newtype BufferRelOffset = BufferRelOffset
  { bufferRelOffsetInternal :: GLuint
  } deriving (Eq, Ord, Num, Real, Enum, Integral)

instance Show BufferRelOffset where
  show (BufferRelOffset n) = "BufferRelOffset" `mappend` show n

newtype BufferStride = BufferStride
  { bufferStrideInternal :: GLsizei
  } deriving (Eq, Ord, Num, Real, Enum, Integral)

instance Show BufferStride where
  show (BufferStride n) = "BufferStride" `mappend` show n

newtype BufferComponentSize = BufferComponentSize
  { bufferComponentSize :: GLint
  } deriving (Eq, Ord, Num, Real, Enum, Integral)

instance Show BufferComponentSize where
  show (BufferComponentSize n) = "BufferComponentSize" `mappend` show n

newtype BufferIndex = BufferIndex
  { bufferIndexInternal :: GLuint
  } deriving (Eq, Ord, Num, Real, Enum, Integral)

instance Show BufferIndex where
  show (BufferIndex n) = "BufferIndex" `mappend` show n

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

-- | The data type 'BufferAttribFlags' indicates what properties we wish an allocated buffer to have.
--   It corresponds to the 'GLbitfield' parameter of the 'BufferStorage' and 'NamedBufferStorage'
--   See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glBufferStorage.xhtml>
--   for details on what the flags actually do.
data BufferAttribFlags = BufferAttribFlags
  { _bufferAttribFlagsMapType       :: BufferMapType
  , _bufferAttribFlagsMapPersistent :: Bool
  , _bufferAttribFlagsMapCoherent   :: Bool
  , _bufferAttribFlagsDynamic       :: Bool
  , _bufferAttribFlagsClientStorage :: Bool
  } deriving (Eq, Ord, Show)

-- | By default we have the buffer be as impermissive as possible.
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

-- | The data type 'BufferMapFlags' indicates what properties we wish a mapped buffer to have.
--   It corresponds to the 'GLbitfield' parameter of the 'MapBufferRange' and 'MapNamedBufferRange' functions.
--   See <https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glMapBufferRange.xhtml>
--   for details on what the flags actually do. Note that the map flags have to match the attribute flags
--   that were set, in certain ways.
data BufferMapFlags = BufferMapFlags
  { _bufferMapFlagsMapType             :: BufferMapType
  , _bufferMapFlagsMapPersistent       :: Bool
  , _bufferMapFlagsMapCoherent         :: Bool
  , _bufferMapFlagsMapInvalidateRange  :: Bool
  , _bufferMapFlagsMapInvalidateBuffer :: Bool
  , _bufferMapFlagsMapFlushExplicit    :: Bool
  , _bufferMapFlagsMapUnsynchronized   :: Bool
  } deriving (Eq, Ord, Show)

-- | By default we have the mapped buffer be as impermissive as possible.
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

-- | A buffer can be created with initial data in it.
initBufferName :: MonadIO m
                 => BufferSize
                 -> BufferAttribFlags
                 -> Ptr ()
                 -> m BufferName
initBufferName size attrib@BufferAttribFlags {..} ptr = do
  bufo@(BufferName n) <- genName'
  glNamedBufferStorage n (bufferSizeInternal size) ptr bitF
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
