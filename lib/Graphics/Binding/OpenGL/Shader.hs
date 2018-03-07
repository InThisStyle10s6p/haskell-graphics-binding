{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Binding.OpenGL.Shader where

import Data.ByteString
import Graphics.Binding.OpenGL.Types
import Graphics.Binding.OpenGL.Utils
import Graphics.GL.Core45
import Foreign.Resource
import Graphics.GL.Types

newtype VertexShader = VertexShader { _vertexShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)

instance ForeignName VertexShader () where
  genName_ _ = VertexShader <$> glCreateShader GL_VERTEX_SHADER
  isName_ (VertexShader n) = unmarshalGLboolean <$> glIsShader n
  deleteName_ (VertexShader n) = glDeleteShader n

newtype FragmentShader = FragmentShader { _fragmentShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)

instance ForeignName FragmentShader () where
  genName_ _ = FragmentShader <$> glCreateShader GL_FRAGMENT_SHADER
  isName_ (FragmentShader n) = unmarshalGLboolean <$> glIsShader n
  deleteName_ (FragmentShader n) = glDeleteShader n

newtype TessControlShader = TessControlShader { _tessControlShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)

instance ForeignName TessControlShader () where
  genName_ _ = TessControlShader <$> glCreateShader GL_TESS_CONTROL_SHADER
  isName_ (TessControlShader n) = unmarshalGLboolean <$> glIsShader n
  deleteName_ (TessControlShader n) = glDeleteShader n

newtype TessEvalShader = TessEvalShader { _tessEvalShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)

instance ForeignName TessEvalShader () where
  genName_ _ = TessEvalShader <$> glCreateShader GL_TESS_EVALUATION_SHADER
  isName_ (TessEvalShader n) = unmarshalGLboolean <$> glIsShader n
  deleteName_ (TessEvalShader n) = glDeleteShader n

newtype ComputeShader = ComputeShader { _computeShaderGLuint :: GLuint } deriving (Eq, Ord, Show, Storable)

instance ForeignName ComputeShader () where
  genName_ _ = ComputeShader <$> glCreateShader GL_COMPUTE_SHADER
  isName_ (ComputeShader n) = unmarshalGLboolean <$> glIsShader n
  deleteName_ (ComputeShader n) = glDeleteShader n

class ForeignName t () => Shader t where
  marshalShaderType :: t -> GLuint
  marshalShaderObject :: t -> GLuint

instance Shader VertexShader where
  marshalShaderType _ = GL_VERTEX_SHADER
  marshalShaderObject = _vertexShaderGLuint

instance Shader TessEvalShader where
  marshalShaderType _ = GL_TESS_EVALUATION_SHADER
  marshalShaderObject = _tessEvalShaderGLuint

instance Shader TessControlShader where
  marshalShaderType _ = GL_TESS_CONTROL_SHADER
  marshalShaderObject = _tessControlShaderGLuint

instance Shader FragmentShader where
  marshalShaderType _ = GL_FRAGMENT_SHADER
  marshalShaderObject = _fragmentShaderGLuint

instance Shader ComputeShader where
  marshalShaderType _ = GL_COMPUTE_SHADER
  marshalShaderObject = _computeShaderGLuint


compileShader :: (Shader t, MonadIO m) => t -> m (Maybe ByteString)
compileShader t = do
  glCompileShader n
  status <- unmarshalGLboolean <$> foreignPoke (glGetShaderiv n GL_COMPILE_STATUS)
  if status
    then return Nothing
    else Just <$> withForeignBufferBS (glGetShaderiv n GL_INFO_LOG_LENGTH) (glGetShaderInfoLog n)
  where
    n = marshalShaderObject t

newtype ShaderDeleteStatus t = ShaderDeleteStatus t

instance Shader t => ForeignRead (ShaderDeleteStatus t) () Bool where
  readR_ (ShaderDeleteStatus shader) _ = unmarshalGLboolean <$> foreignPoke (glGetShaderiv (marshalShaderObject shader) GL_DELETE_STATUS)

newtype ShaderInfoLog t = ShaderInfoLog t

instance Shader t => ForeignRead (ShaderInfoLog t) () ByteString where
  readR_ (ShaderInfoLog shader) _ = withForeignBufferBS (glGetShaderiv n GL_INFO_LOG_LENGTH) (glGetShaderInfoLog n)
    where
      n = marshalShaderObject shader

newtype ShaderSource t = ShaderSource { _unShaderSource :: t }

instance Shader t => ForeignRead (ShaderSource t) () ByteString where
  readR_ (ShaderSource shader) _ = withForeignBufferBS (glGetShaderiv n GL_SHADER_SOURCE_LENGTH) $ glGetShaderSource n
    where
      n = marshalShaderObject shader

instance Shader t => ForeignWrite (ShaderSource t) () ByteString where
  writeR_ (ShaderSource shader) _ src = withByteString src
>>>>>>> 1573bf8
    ( \srcPtr srcLength -> with srcPtr $
      \srcPtrBuf -> with srcLength $
      \srcLengthBuf -> glShaderSource (marshalShaderObject shader) 1 srcPtrBuf srcLengthBuf
    ) >> return (ShaderSource shader)

instance Shader t => ForeignUpdate (ShaderSource t) () ByteString where
