{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Binding
  ( module X
  , bindFullDynamicUniformBuffer
  , DynamicBuffer
  , FullBufferWrite(..)
  ) where

import Graphics.GL.Types as X
  ( GLfloat
  , GLint
  , GLuint
  , GLboolean
  , GLdouble
  )

import Graphics.Binding.GLFW.Window as X

import Graphics.Binding.OpenGL.BufferObject as X
  ( BufferName
  , BufferSize(..)
  , BufferOffset(..)
  , BufferRelOffset(..)
  , BufferStride(..)
  , BufferComponentSize(..)
  , BufferIndex(..)
  , IntegerHandling(..)
  , BufferMapType(..)
  , BufferAttribFlags(..)
  , mapType
  , mapPersistent
  , mapCoherent
  , dynamic
  , clientStorage
  , defaultBufferAttribFlags
  , BufferMapFlags(..)
  , mapType
  , mapPersistent
  , mapCoherent
  , mapInvalidateRange
  , mapInvalidateBuffer
  , mapFlushExplicit
  , mapUnsynchronized
  , defaultBufferMapFlags
  , initBufferName
  , bufferSubData
  , mapBuffer
  , mapBufferRange
  , clearBufferSubData
  , copyBufferSubData
  )

import Graphics.Binding.OpenGL.Framebuffer as X
  ( Framebuffer
  , defaultFramebuffer
  , FramebufferDefault(..)
  , Renderbuffer
  , RenderbufferInternalFormat
  , renderbufferStorageMultisample
  , renderbufferStorage
  , FramebufferAttachment(..)
  , RenderbufferAttachPoint(..)
  , MipmapLevel
  , TextureLayer
  , TextureAttachPoint(..)
  , TextureLayerAttachPoint(..)
  , FramebufferStatus(..)
  , FramebufferStatusError(..)
  , FramebufferTarget(..)
  )

import Graphics.Binding.OpenGL.PrimitiveUniform as X
  ( UniformLocation(..)
  , PrimUniform(..)
  , primMarshal
  , primMarshalArray
  )

import Graphics.Binding.OpenGL.Rendering as X
  ( ClearBuffer(..)
  , clearBufferColor
  , clearBufferStencil
  , clearBufferDepth
  , defaultClearBuffer
  , PrimitiveMode(..)
  , IndexType(..)
  , clear
  , drawElements
  )

import Graphics.Binding.OpenGL.Shader as X
  ( Program
  , CurrentProgram(..)
  , ProgramShaderComponent(..)
  , linkProgram
  , ProgramLike
  , ShaderStageType(..)
  , ShaderStage
  , ShaderType(showShaderType)
  , ShaderPipeline
  , ShaderStageSet(..)
  , ActivePipeline(..)
  , ShaderPipelineSpec(..)
  , shaderPipelineSpecVertexShader
  , shaderPipelineSpecTessControlShader
  , shaderPipelineSpecTessEvalShader
  , shaderPipelineSpecGeometryShader
  , shaderPipelineSpecFragmentShader
  )

import Graphics.Binding.OpenGL.Synchro as X
  ( GLFence
  , deleteGLFence
  , lockGLFence
  , waitGLFence
  )

import Graphics.Binding.OpenGL.Texture as X
  ( TextureUnit(..)
  , TextureTarget1D(..)
  , TextureTarget2D(..)
  , TextureTarget3D(..)
  , Texture2DMultisample(..)
  , Texture2DMultisampleArray(..)
  , Texture1DAttrib(..)
  , Texture2DAttrib(..)
  , Texture3DAttrib(..)
  , bufferFormat
  , mipmapLevel
  , bufferWidth
  , bufferHeight
  , bufferDepth
  , TextureObject
  , PixelFormat(..)
  , Pixel1DAttrib(..)
  , Pixel2DAttrib(..)
  , Pixel3DAttrib(..)
  , pixelFormat
  , pixelType
  , pixelWidth
  , pixelHeight
  , pixelDepth
  , pixelXOffset
  , pixelYOffset
  , pixelZOffset
  , TextureParameter(..)
  , TextureTarget( TextureConfig
                 , PixelConfig
                 , createTexture
                 , textureSubMap
                 , showTarget
                 )
  , TextureSampler(..)
  , textureParameterf
  , textureParameteri
  , primTextureUnitBind_
  , primTextureUnitBind
  )

import Graphics.Binding.OpenGL.Types as X
  ( GLSized(..)
  , gSize
  , GLWritable(..)
  , gPoke
  , GLReadable(..)
  , gPeek
  , GLDataType(..)
  , SizedFormat(..)
  , BufferAccess(..)
  , Capability(..)
  , BufferBindPoint(..)
  , SamplerType(..)
  , GLDeleteStatus(..)
  , GLInfoLog(..)
  , GLValidateStatus(..)
  , GLCompilationStatus(..)
  )

import Graphics.Binding.OpenGL.Uniform as X
  ( UniformBufferBindingLocation(..)
  , DefaultBlockUniform(..)
  , defaultBlockUniform
  , bindUniformBuffer
  )

import Graphics.Binding.OpenGL.VertexArray as X
  ( VertexArrayObject
  , AttribLocation(..)
  , UniformBlockLocation(..)
  , TextureSamplerLocation(..)
  , ActiveVertexArrayObject(..)
  , vertexArrayAttribBinding
  , vertexArrayVertexBuffer
  , vertexArrayAttribFormat
  , vertexArrayAttribCapability
  , VertexArrayAttribCategory(..)
  , bindElementBuffer
  )

import Graphics.Binding.OpenGL.Window as X
  ( Face(..)
  , Color4(..)
  , DepthFunc(..)
  , DebugSource(..)
  , DebugType(..)
  , DebugSeverity(..)
  , DebugID(..)
  , DebugCallbackFun
  , FaceCullMode(..)
  , color4
  , ClearColor(..)
  , DepthTest(..)
  , DebugMessageCallback(..)
  , simpleDebugFunc
  )


------
import Control.Monad.IO.Class
import Data.Typeable
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Resource
import Graphics.Binding.OpenGL.BufferObject
import Graphics.GL.Core45

-- | A 'DynamicBuffer a' is the name of a buffer that by default has only the dynamic bit set,
--   and can only be mapped with the dynamic bit.
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

-- | If we have declared how the contents of a dynamic buffer should be laid out
--   in memory for OpenGL to use, via an instance of 'GLWritable', then we can
--   use this type to have 'BufferSubData' write a value 'a' into the
--   corresponding OpenGL buffer.
data FullBufferWrite = FullBufferWrite
  deriving (Eq, Ord, Show)

instance GLWritable a => ForeignWrite FullBufferWrite (DynamicBuffer a) a where
  writeR_ _ b@(DynamicBuffer n) a = allocaBytes size $ \ptr -> do
    gPoke ptr a
    bufferSubData n (fromIntegral size) 0 (castPtr ptr) >> return b
    where
      size = gSize (Proxy :: Proxy a)

bindFullDynamicUniformBuffer :: forall a m b. (MonadIO m, GLWritable a) => b -> UniformBufferBindingLocation -> DynamicBuffer a -> m b
bindFullDynamicUniformBuffer b loc (DynamicBuffer name) = bindUniformBuffer loc name 0 (gSize (Proxy :: Proxy a)) >> return b

