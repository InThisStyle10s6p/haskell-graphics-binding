{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Graphics.Binding.OpenGL.Program where

import Graphics.GL.Core45
import Graphics.GL.Types
import Graphics.Binding.OpenGL.Utils
import Graphics.Binding.OpenGL.Types
import Graphics.Binding.OpenGL.Shader
import Data.ByteString
import Foreign.Resource

newtype Program = Program { getProgramGLuint :: GLuint } deriving (Eq, Ord, Show)

instance ForeignName Program () where
  genName_ _ = Program <$> glCreateProgram
  isName_ (Program n) = unmarshalGLboolean <$> glIsProgram n
  deleteName_ (Program n) = glDeleteProgram n

data CurrentProgram = CurrentProgram
  deriving (Eq, Ord, Show)

data ProgramDeleteStatus = ProgramDeleteStatus
  deriving (Eq, Ord, Show)

data ProgramValidateStatus = ProgramValidateStatus
  deriving (Eq, Ord, Show)

data ProgramShaderComponent = ProgramShaderComponent
  deriving (Eq, Ord, Show)

instance ForeignRead Program ProgramDeleteStatus Bool where
  readR_ (Program n) _ = unmarshalGLboolean <$> foreignPoke (glGetProgramiv n GL_DELETE_STATUS)

instance ForeignRead Program ProgramValidateStatus (Maybe ByteString) where
  readR_ (Program n) _ = do
    glValidateProgram n
    status <- unmarshalGLboolean <$> foreignPoke (glGetProgramiv n GL_VALIDATE_STATUS)
    if status
      then return Nothing
      else Just <$> withForeignBufferBS (glGetProgramiv n GL_INFO_LOG_LENGTH) (glGetProgramInfoLog n)

newtype ShaderAttach t = ShaderAttach { _unShaderAttach :: t }

instance Shader t => ForeignWrite Program () (ShaderAttach t) where
  writeR_ prog@(Program n) _ (ShaderAttach t) = glAttachShader n (marshalShaderObject t) >> return prog

linkProgram :: MonadIO m => Program -> m (Maybe ByteString)
linkProgram (Program n) = liftIO $ do
  glLinkProgram n
  status <- unmarshalGLboolean <$> foreignPoke (glGetProgramiv n GL_LINK_STATUS)
  if status
    then return Nothing
    else Just <$> withForeignBufferBS (glGetProgramiv n GL_INFO_LOG_LENGTH) (glGetProgramInfoLog n)

instance ForeignWrite CurrentProgram () (Maybe Program) where
  writeR_ _ _ = \case
    Nothing          -> glUseProgram 0 >> return CurrentProgram
    Just (Program n) -> glUseProgram n >> return CurrentProgram

instance ForeignRead CurrentProgram () (Maybe Program) where
  readR_ _ _ = do
    n <- foreignPoke (glGetInteger64v GL_CURRENT_PROGRAM)
    if n == 0
      then return Nothing
      else return . Just . Program . fromIntegral $ n

instance ForeignUpdate CurrentProgram () (Maybe Program) where
