{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Graphics.Binding.OpenGL.VertexArray where

import Graphics.Binding.OpenGL.BufferObject
import Graphics.Binding.OpenGL.Types
import Graphics.Binding.OpenGL.Utils
import Graphics.GL.Core45
import Graphics.GL.Types
import Foreign.Marshal.Array
import Foreign.Resource

newtype VertexArrayObject = VertexArrayObject
  { getVertexArrayObjectGLuint :: GLuint
  } deriving (Eq, Ord, Storable)

instance Show VertexArrayObject where
  show (VertexArrayObject n) = "VertexArrayObject " `mappend` show n

instance ForeignName VertexArrayObject () where
  genNames_ n _ = fmap (fmap VertexArrayObject) . liftIO . allocaArray n $
    \ptr -> glCreateVertexArrays (fromIntegral n) ptr >> peekArray n ptr

  isName_ (VertexArrayObject n) = unmarshalGLboolean <$> glIsBuffer n

  deleteNames_ ns = liftIO . withArrayLen ns $ \len ptr -> glDeleteVertexArrays (fromIntegral len) (castPtr ptr)

newtype AttribLocation = AttribLocation
  { getAttribLocationGLuint :: GLuint
  } deriving (Eq, Ord, Num)

instance Show AttribLocation where
  show (AttribLocation n) = "AttribLocation " `mappend` show n

newtype UniformBlockLocation = UniformBlockLocation
  { _getUniformBlockLocation :: GLuint
  } deriving (Eq, Ord, Num)

instance Show UniformBlockLocation where
  show (UniformBlockLocation n) = "UniformBlockLocation " `mappend` show n

newtype TextureSamplerLocation = TextureSamplerLocation
  { _getTextureSamplerLocation :: GLuint
  } deriving (Eq, Ord, Num)

instance Show TextureSamplerLocation where
  show (TextureSamplerLocation n) = "TextureSamplerLocation " `mappend` show n


data ActiveVertexArrayObject = ActiveVertexArrayObject
  deriving (Eq, Ord, Show)

instance ForeignRead () ActiveVertexArrayObject (Maybe VertexArrayObject) where
  readR_ _ _ = do
    n <- foreignPoke (glGetInteger64v GL_ARRAY_BUFFER_BINDING)
    if n == 0
      then return Nothing
      else return . Just . VertexArrayObject . fromIntegral $ n

instance ForeignWrite () ActiveVertexArrayObject (Maybe VertexArrayObject) where
  writeR_ _ _ = \case
    Nothing -> return ActiveVertexArrayObject
    Just (VertexArrayObject n) -> glBindVertexArray n >> return ActiveVertexArrayObject

vertexArrayAttribBinding :: MonadIO m => VertexArrayObject -> AttribLocation -> AttribLocation -> m ()
vertexArrayAttribBinding (VertexArrayObject n) (AttribLocation attribindex) (AttribLocation bindindex)
  = glVertexArrayAttribBinding n attribindex bindindex

vertexArrayVertexBuffer :: MonadIO m => VertexArrayObject -> AttribLocation -> BufferName -> BufferOffset -> BufferStride -> m ()
vertexArrayVertexBuffer (VertexArrayObject n) (AttribLocation bindindx) (BufferName bufobj) offset stride
  = glVertexArrayVertexBuffer n bindindx bufobj (fromIntegral offset) (fromIntegral stride)

vertexArrayAttribFormat :: MonadIO m => VertexArrayObject -> AttribLocation -> BufferComponentSize -> GLDataType -> IntegerHandling -> BufferRelOffset -> m ()
vertexArrayAttribFormat (VertexArrayObject vaobj) (AttribLocation attribindx) size typ integerHandling relOffset
  = glVertexArrayAttribFormat vaobj attribindx (fromIntegral size) (marshalGLDataType typ) handleFlag (fromIntegral relOffset)
  where
    handleFlag = case integerHandling of
      Normalized -> GL_TRUE
      NotNormalized -> GL_FALSE

vertexArrayAttribCapability :: MonadIO m => VertexArrayObject -> AttribLocation -> Capability -> m ()
vertexArrayAttribCapability (VertexArrayObject n) (AttribLocation loc) = \case
  Enabled  -> glEnableVertexArrayAttrib n loc
  Disabled -> glDisableVertexArrayAttrib n loc

bindElementBuffer :: MonadIO m => VertexArrayObject -> BufferName -> m ()
bindElementBuffer (VertexArrayObject n) (BufferName m) = glVertexArrayElementBuffer n m
