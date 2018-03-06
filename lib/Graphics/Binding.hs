module Graphics.Binding (module X) where

import Graphics.GL.Types as X
  ( GLfloat
  , GLint
  , GLuint
  , GLboolean
  , GLdouble
  )

import Graphics.Binding.OpenGL.Types as X
  ( BufferObjectMapType(..)
  , BufferObjectSize(..)
  , BufferObjectOffset(..)
  , BufferObjectRelOffset(..)
  , BufferObjectStride(..)
  , BufferObjectComponentSize(..)
  , defaultBufferAttribFlags
  , BufferObject
  , IntegerHandling(..)
  , mapType
  , mapPersistent
  , mapCoherent
  , mapInvalidateRange
  , mapInvalidateBuffer
  , mapFlushExplicit
  , mapUnsynchronized
  , mapDynamic
  , clientStorage
  , DefaultBlock
  , Uniform(..)
  , UniformBlock(..)
  , bindBlock
  , PersistentBuffer
  , VertexShader
  , TessEvalShader
  , TessControlShader
  , FragmentShader
  , ComputeShader
  , Shader
  , IndexType(..)
  , PrimitiveMode(..)
  , ClearBuffer()
  , clearBufferColor
  , clearBufferStencil
  , clearBufferDepth
  , defaultClearBuffer
  , Program
  , TextureTarget(..)
  , createTexture
  , TextureTarget1D(..)
  , TextureTarget2D(..)
  , TextureTarget3D(..)
  , Texture1DAttrib(..)
  , Texture2DAttrib(..)
  , Texture3DAttrib(..)
  , Pixel1DAttrib(..)
  , Pixel2DAttrib(..)
  , Pixel3DAttrib(..)
  , mipmapLevel
  , bufferFormat
  , pixelFormat
  , bufferWidth
  , bufferHeight
  , bufferDepth
  , PixelFormat(..)
  , TextureParameter(..)
  , TextureObject
  , genTextureName
  , genTextureNames
  , TextureUnit(..)
  , TextureSampler(..)
  , texture
  , SizedFormat(..)
  , GLDataType(..)
  , Capability(..)
  , SamplerType(..)
  , VertexArrayObject
  , AttribLocation(..)
  , UniformLocation(..)
  , PrimUniform(..)
  , primMarshal
  , primMarshalArray
  , DepthFunc(..)
  , Color4(..)
  , DebugSource(..)
  , DebugType(..)
  , DebugSeverity(..)
  , DebugID(..)
  , DebugCallbackFun
  , Face(..)
  , GLSized(..)
  , gSize
  , GLWritable(..)
  , gPoke
  , GLReadable(..)
  , gPeek
  , UniformBlock(..)
  , bindBlock
  , WritableBuffer
  )

import Graphics.Binding.OpenGL.BufferObject as X
  ( initBufferObject
  , bufferSubData
  , mapBuffer
  , unmapBuffer
  , clearBufferSubData
  , copyBufferSubData
  , mapBufferRange
  , fencePersistentBuffer
  , persistentBufferWrite
  , writableBufferWrite
  )

import Graphics.Binding.OpenGL.Shader as X
  ( compileShader
  , shaderDeleteStatus
  , shaderSource
  )
import Graphics.Binding.OpenGL.Program as X
  ( programDeleteStatus
  , attachShader
  , linkProgram
  , validateProgram
  , currentProgram
  , useProgram
  )

import Graphics.Binding.OpenGL.Uniform as X
  ( uniformBlockBinding
  , bindPersistentBufferToPoint
  , bindFullPersistentBufferToPoint
  , bindFullWritableBufferToPoint
  )

import Graphics.Binding.OpenGL.Texture as X
  ( primTextureUnitBind
  , textureParameterf
  , textureParameteri
  , primTextureUnitBind_
  , primTextureUnitBind
  )
import Graphics.Binding.OpenGL.VertexArray as X
  ( currentVertexArrayObject
  , vertexArrayAttribBinding
  , vertexArrayVertexBuffer
  , vertexArrayAttribFormat
  , vertexArrayAttribCapability
  , bindElementBuffer
  )

import Graphics.Binding.OpenGL.Window as X
  ( depthFunc
  , color4
  , clearColor
  , debugMessageCallback
  , cullFace
  , simpleDebugFunc
  )

import Graphics.Binding.OpenGL.Rendering as X
  ( drawElements
  , clear
  )

import Graphics.Binding.OpenGL.Synchro as X
import Graphics.Binding.GLFW.Window as X

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


