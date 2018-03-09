{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Graphics.Binding.OpenGL.PrimitiveUniform where

import           Control.Monad.IO.Class
import qualified Data.Vector.Storable as VS
import           Graphics.Binding.OpenGL.Shader
import           Graphics.Binding.OpenGL.Utils
import           Graphics.GL.Core45
import           Graphics.GL.Types
import           Linear

-- * Primitive uniform operations.

newtype UniformLocation = UniformLocation
  { uniformLocationInternal :: GLint
  } deriving (Eq, Ord, Num)

instance Show UniformLocation where
  show (UniformLocation n) = "UniformLocation " `mappend` show n

class PrimUniform a where
  primMarshal_ :: MonadIO m => Program -> UniformLocation -> a -> m ()
  primMarshalArray_ :: MonadIO m => Program -> UniformLocation -> VS.Vector a -> m ()

primMarshal :: (PrimUniform a, MonadIO m) => UniformLocation -> Program -> a -> m ()
primMarshal = flip primMarshal_

primMarshalArray :: (PrimUniform a, MonadIO m) => UniformLocation -> Program -> VS.Vector a -> m ()
primMarshalArray = flip primMarshalArray_

instance PrimUniform GLint where
  primMarshal_ (Program n) (UniformLocation m) = glProgramUniform1i n m
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ glProgramUniform1iv n m

instance PrimUniform GLuint where
  primMarshal_ (Program n) (UniformLocation m) = glProgramUniform1ui n m
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ glProgramUniform1uiv n m

instance PrimUniform GLfloat where
  primMarshal_ (Program n) (UniformLocation m) = glProgramUniform1f n m
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ glProgramUniform1fv n m

instance PrimUniform GLdouble where
  primMarshal_ (Program n) (UniformLocation m) = glProgramUniform1d n m
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ glProgramUniform1dv n m

instance PrimUniform (V1 GLint) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform1iv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform1iv n m len (castPtr ptr)

instance PrimUniform (V1 GLuint) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform1uiv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform1uiv n m len (castPtr ptr)

instance PrimUniform (V1 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform1fv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform1fv n m len (castPtr ptr)

instance PrimUniform (V1 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform1dv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform1dv n m len (castPtr ptr)

instance PrimUniform (V2 GLint) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform2iv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform2iv n m len (castPtr ptr)

instance PrimUniform (V2 GLuint) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform2uiv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform2uiv n m len (castPtr ptr)

instance PrimUniform (V2 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform2fv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform2fv n m len (castPtr ptr)

instance PrimUniform (V2 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform2dv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform2dv n m len (castPtr ptr)

instance PrimUniform (V3 GLint) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform3iv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform3iv n m len (castPtr ptr)

instance PrimUniform (V3 GLuint) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform3uiv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform3uiv n m len (castPtr ptr)

instance PrimUniform (V3 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform3fv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform3fv n m len (castPtr ptr)

instance PrimUniform (V3 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform3dv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform3dv n m len (castPtr ptr)

instance PrimUniform (V4 GLint) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform4iv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform4iv n m len (castPtr ptr)

instance PrimUniform (V4 GLuint) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform4uiv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform4uiv n m len (castPtr ptr)

instance PrimUniform (V4 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform4fv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform4fv n m len (castPtr ptr)

instance PrimUniform (V4 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ glProgramUniform4dv n m 1 . castPtr
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniform4dv n m len (castPtr ptr)

-- Column major!

instance PrimUniform (M22 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix2fv n m 1 GL_FALSE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix2fv n m len GL_FALSE (castPtr ptr)

instance PrimUniform (M23 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix2x3fv n m 1 GL_FALSE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix2x3fv n m len GL_FALSE (castPtr ptr)

instance PrimUniform (M24 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix2x4fv n m 1 GL_FALSE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix2x4fv n m len GL_FALSE (castPtr ptr)

instance PrimUniform (M32 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix3x2fv n m 1 GL_FALSE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix3x2fv n m len GL_FALSE (castPtr ptr)

instance PrimUniform (M33 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix3fv n m 1 GL_FALSE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix3fv n m len GL_FALSE (castPtr ptr)

instance PrimUniform (M34 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix3x4fv n m 1 GL_FALSE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix3x4fv n m len GL_FALSE (castPtr ptr)

instance PrimUniform (M42 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix4x2fv n m 1 GL_FALSE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix4x2fv n m len GL_FALSE (castPtr ptr)

instance PrimUniform (M43 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix4x3fv n m 1 GL_FALSE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix4x3fv n m len GL_FALSE (castPtr ptr)

instance PrimUniform (M44 GLfloat) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix4fv n m 1 GL_FALSE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix4fv n m len GL_FALSE (castPtr ptr)

instance PrimUniform (M22 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix2dv n m 1 GL_FALSE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix2dv n m len GL_FALSE (castPtr ptr)

instance PrimUniform (M23 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix2x3dv n m 1 GL_FALSE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix2x3dv n m len GL_FALSE (castPtr ptr)

instance PrimUniform (M24 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix2x4dv n m 1 GL_FALSE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix2x4dv n m len GL_FALSE (castPtr ptr)

instance PrimUniform (M32 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix3x2dv n m 1 GL_FALSE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix3x2dv n m len GL_FALSE (castPtr ptr)

instance PrimUniform (M33 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix3dv n m 1 GL_FALSE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix3dv n m len GL_FALSE (castPtr ptr)

instance PrimUniform (M34 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix3x4dv n m 1 GL_FALSE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix3x4dv n m len GL_FALSE (castPtr ptr)

instance PrimUniform (M42 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix4x2dv n m 1 GL_FALSE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix4x2dv n m len GL_FALSE (castPtr ptr)

instance PrimUniform (M43 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix4x3dv n m 1 GL_FALSE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix4x3dv n m len GL_FALSE (castPtr ptr)

instance PrimUniform (M44 GLdouble) where
  primMarshal_ (Program n) (UniformLocation m) v = liftIO . with v $ \ptr -> glProgramUniformMatrix4dv n m 1 GL_FALSE (castPtr ptr)
  primMarshalArray_ (Program n) (UniformLocation m) vs = unsafeWithVecLen vs $ \len ptr -> glProgramUniformMatrix4dv n m len GL_FALSE (castPtr ptr)

