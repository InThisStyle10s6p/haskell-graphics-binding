{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Graphics.Binding.OpenGL.Shader where

import Data.ByteString
import Data.ByteString.Char8 (useAsCString)
import Graphics.Binding.OpenGL.Types
import Graphics.Binding.OpenGL.Utils
import Graphics.GL.Core45
import Foreign.Resource
import Graphics.GL.Types
import Data.Typeable
import Data.Foldable
import Data.Monoid

-- * Bare program related things.
newtype Program = Program
  { getProgramGLuint :: GLuint
  } deriving (Eq, Ord, Show, Storable)

instance ForeignName Program () where
  genName_ _ = Program <$> glCreateProgram
  isName_ (Program n) = unmarshalGLboolean <$> glIsProgram n
  deleteName_ (Program n) = glDeleteProgram n

data CurrentProgram = CurrentProgram
  deriving (Eq, Ord, Show)

data ProgramShaderComponent = ProgramShaderComponent
  deriving (Eq, Ord, Show)

instance ForeignRead GLDeleteStatus Program Bool where
  readR_ _ (Program n) = unmarshalGLboolean <$> foreignPoke (glGetProgramiv n GL_DELETE_STATUS)

instance ForeignRead GLInfoLog Program (Maybe ByteString) where
  readR_ _ (Program n) = do
    loglen <- foreignPoke $ glGetProgramiv n GL_INFO_LOG_LENGTH
    if loglen == 0
      then return Nothing
      else Just <$> withForeignBufferBS (glGetProgramiv n GL_INFO_LOG_LENGTH) (glGetProgramInfoLog n)

instance ForeignRead GLValidateStatus Program (Maybe ByteString) where
  readR_ _ (Program n) = do
    glValidateProgram n
    status <- unmarshalGLboolean <$> foreignPoke (glGetProgramiv n GL_VALIDATE_STATUS)
    if status
      then return Nothing
      else Just <$> withForeignBufferBS (glGetProgramiv n GL_INFO_LOG_LENGTH) (glGetProgramInfoLog n)

instance ForeignRead GLCompilationStatus Program (Maybe ByteString) where
  readR_ _ (Program n) = do
    status <- unmarshalGLboolean <$> foreignPoke (glGetProgramiv n GL_COMPILE_STATUS)
    if status
      then return Nothing
      else Just <$> withForeignBufferBS (glGetProgramiv n GL_INFO_LOG_LENGTH) (glGetProgramInfoLog n)

linkProgram :: MonadIO m => Program -> m (Maybe ByteString)
linkProgram (Program n) = liftIO $ do
  glLinkProgram n
  status <- unmarshalGLboolean <$> foreignPoke (glGetProgramiv n GL_LINK_STATUS)
  if status
    then return Nothing
    else Just <$> withForeignBufferBS (glGetProgramiv n GL_INFO_LOG_LENGTH) (glGetProgramInfoLog n)

instance ForeignWrite () CurrentProgram (Maybe Program) where
  writeR_ _ _ = \case
    Nothing          -> glUseProgram 0 >> return CurrentProgram
    Just (Program n) -> glUseProgram n >> return CurrentProgram

instance ForeignRead () CurrentProgram (Maybe Program) where
  readR_ _ _ = do
    n <- foreignPoke (glGetInteger64v GL_CURRENT_PROGRAM)
    if n == 0
      then return Nothing
      else return . Just . Program . fromIntegral $ n

instance ForeignUpdate () CurrentProgram (Maybe Program) where

class ProgramLike t where
  toProgram :: t -> Program

-- * Shader stages

data ShaderStageType
  = VertexShader
  | TessControlShader
  | TessEvalShader
  | GeometryShader
  | FragmentShader
  | ComputeShader
  deriving (Eq, Ord, Show, Typeable)

newtype ShaderStage (a :: ShaderStageType) = ShaderStage
  { _shaderStageName :: Program
  } deriving (Eq, Ord, Storable)

class ShaderType (a :: ShaderStageType) where
  marshalShaderType :: c a -> GLenum
  marshalShaderStage :: c a -> GLbitfield
  showShaderType :: c a -> String

instance ShaderType 'VertexShader where
  marshalShaderType  _ = GL_VERTEX_SHADER
  marshalShaderStage _ = GL_VERTEX_SHADER_BIT
  showShaderType     _ = show VertexShader
instance ShaderType 'TessControlShader where
  marshalShaderType  _ = GL_TESS_CONTROL_SHADER
  marshalShaderStage _ = GL_TESS_CONTROL_SHADER_BIT
  showShaderType     _ = show TessControlShader
instance ShaderType 'TessEvalShader where
  marshalShaderType  _ = GL_TESS_EVALUATION_SHADER
  marshalShaderStage _ = GL_TESS_EVALUATION_SHADER_BIT
  showShaderType     _ = show TessEvalShader
instance ShaderType 'GeometryShader where
  marshalShaderType  _ = GL_GEOMETRY_SHADER
  marshalShaderStage _ = GL_GEOMETRY_SHADER_BIT
  showShaderType     _ = show GeometryShader
instance ShaderType 'FragmentShader where
  marshalShaderType  _ = GL_FRAGMENT_SHADER
  marshalShaderStage _ = GL_FRAGMENT_SHADER_BIT
  showShaderType     _ = show FragmentShader
instance ShaderType 'ComputeShader where
  marshalShaderType  _ = GL_COMPUTE_SHADER
  marshalShaderStage _ = GL_COMPUTE_SHADER_BIT
  showShaderType     _ = show ComputeShader

instance ShaderType t => ProgramLike (ShaderStage t) where
  toProgram (ShaderStage n) = n

instance ShaderType t => Show (ShaderStage t) where
  show t = "ShaderStage " <> showShaderType t

instance ShaderType t => ForeignName (ShaderStage t) ByteString where
  genName_ src = useAsCString src $
    \srcPtr -> with srcPtr $
    fmap (ShaderStage . Program) . glCreateShaderProgramv n 1 . castPtr
    where
      n = marshalShaderType (Proxy :: Proxy t)
  isName_ = isName_ . toProgram
  deleteName_ = deleteName_ . toProgram

instance ShaderType t => ForeignRead GLInfoLog (ShaderStage t) (Maybe ByteString) where
  readR_ t = readR_ t . toProgram

instance ShaderType t => ForeignRead GLCompilationStatus (ShaderStage t) (Maybe ByteString) where
  readR_ t = readR_ t . toProgram

instance ShaderType t => ForeignRead GLValidateStatus (ShaderStage t) (Maybe ByteString) where
  readR_ t = readR_ t . toProgram

instance ShaderType t => ForeignRead GLDeleteStatus (ShaderStage t) Bool where
  readR_ t = readR_ t . toProgram

newtype ShaderPipeline = ShaderPipeline
  { _getShaderPipelineGLuint :: GLuint
  } deriving (Eq, Ord, Show)

-- NB supposedly CreateProgramPipelines works with BindProgramPipeline.
-- We shall see.
instance ForeignName ShaderPipeline () where
  genNames_ = genericGLCreate ShaderPipeline glCreateProgramPipelines
  isName_   = genericGLIsName _getShaderPipelineGLuint glIsProgramPipeline
  deleteNames_ = genericGLDeleteNames _getShaderPipelineGLuint glDeleteProgramPipelines

data ShaderStageSet (t :: ShaderStageType) = ShaderStageSet
  deriving (Eq, Ord, Show)

instance ShaderType t => ForeignWrite (ShaderStageSet t) ShaderPipeline (ShaderStage t) where
  writeR_ _ s@(ShaderPipeline n) t@(ShaderStage (Program m)) = glUseProgramStages n (marshalShaderStage t) m >> return s

data ActivePipeline = ActivePipeline
  deriving (Eq, Ord, Show)

instance ForeignWrite () ActivePipeline (Maybe ShaderPipeline) where
  writeR_ _ _ = \case
    Nothing -> glBindProgramPipeline 0 >> return ActivePipeline
    Just (ShaderPipeline n) -> glBindProgramPipeline n >> return ActivePipeline

instance ForeignRead GLValidateStatus ShaderPipeline (Maybe ByteString) where
  readR_ _ (ShaderPipeline n) = do
    glValidateProgramPipeline n
    status <- unmarshalGLboolean <$> foreignPoke (glGetProgramPipelineiv n GL_VALIDATE_STATUS)
    if status
      then return Nothing
      else Just <$> withForeignBufferBS (glGetProgramPipelineiv n GL_INFO_LOG_LENGTH) (glGetProgramPipelineInfoLog n)

data ShaderPipelineSpec = ShaderPipelineSpec
  { _shaderPipelineSpecVertexShader      :: ShaderStage 'VertexShader
  , _shaderPipelineSpecTessControlShader :: Maybe (ShaderStage 'TessControlShader)
  , _shaderPipelineSpecTessEvalShader    :: Maybe (ShaderStage 'TessEvalShader)
  , _shaderPipelineSpecGeometryShader    :: Maybe (ShaderStage 'GeometryShader)
  , _shaderPipelineSpecFragmentShader    :: ShaderStage 'FragmentShader
  } deriving (Eq, Ord, Show)

instance ForeignWrite () ShaderPipeline ShaderPipelineSpec where
  writeR_ _ t ShaderPipelineSpec {..} = do
    t ~& ShaderStageSet .$= _shaderPipelineSpecVertexShader
    traverse_ (writeR ShaderStageSet t) _shaderPipelineSpecTessControlShader
    traverse_ (writeR ShaderStageSet t) _shaderPipelineSpecTessEvalShader
    traverse_ (writeR ShaderStageSet t) _shaderPipelineSpecGeometryShader
    t ~& ShaderStageSet .$= _shaderPipelineSpecFragmentShader
    return t
