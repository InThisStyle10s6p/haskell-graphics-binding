{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Graphics.Binding.OpenGL.Framebuffer where

import Foreign
import Foreign.Resource
import Graphics.Binding.OpenGL.Texture
import Graphics.Binding.OpenGL.Types
import Graphics.Binding.OpenGL.Utils
import Graphics.GL.Core45
import Graphics.GL.Types

newtype Framebuffer = Framebuffer
  { getFramebufferGLuint :: GLuint
  } deriving (Eq, Ord, Storable)

instance Show Framebuffer where
  show (Framebuffer n) = "Framebuffer " `mappend` show n

instance ForeignName Framebuffer () where
  genNames_ = genericGLCreate Framebuffer glCreateFramebuffers

  isName_ = genericGLIsName getFramebufferGLuint glIsFramebuffer

  deleteNames_ = genericGLDeleteNames getFramebufferGLuint glDeleteFramebuffers

defaultFramebuffer :: Framebuffer
defaultFramebuffer = Framebuffer 0

data FramebufferDefault
  = FramebufferDefaultWidth
  | FramebufferDefaultHeight
  | FramebufferDefaultLayers
  | FramebufferDefaultSamples
  | FramebufferDefaultFixedSampleLocations
  deriving (Eq, Ord, Show)

marshalFramebufferParameter :: FramebufferDefault -> GLenum
marshalFramebufferParameter = \case
  FramebufferDefaultWidth                -> GL_FRAMEBUFFER_DEFAULT_WIDTH
  FramebufferDefaultHeight               -> GL_FRAMEBUFFER_DEFAULT_HEIGHT
  FramebufferDefaultLayers               -> GL_FRAMEBUFFER_DEFAULT_LAYERS
  FramebufferDefaultSamples              -> GL_FRAMEBUFFER_DEFAULT_SAMPLES
  FramebufferDefaultFixedSampleLocations -> GL_FRAMEBUFFER_DEFAULT_FIXED_SAMPLE_LOCATIONS

--- Cannot be used with the default framebuffer!
instance ForeignWrite FramebufferDefault Framebuffer Int where
  writeR_ n t@(Framebuffer m) l = glNamedFramebufferParameteri m (marshalFramebufferParameter n) (fromIntegral l) >> return t

----

newtype Renderbuffer = Renderbuffer
  { getRenderbufferGLuint :: GLuint
  } deriving (Eq, Ord, Storable)

instance Show Renderbuffer where
  show (Renderbuffer n) = "Renderbuffer " `mappend` show n

instance ForeignName Renderbuffer () where
  genNames_ = genericGLCreate Renderbuffer glCreateRenderbuffers

  isName_ = genericGLIsName getRenderbufferGLuint glIsRenderbuffer

  deleteNames_ = genericGLDeleteNames getRenderbufferGLuint glDeleteRenderbuffers

--- 9.2.5 lists required renderbuffer formats
type RenderbufferInternalFormat = SizedFormat

renderbufferStorageMultisample :: MonadIO m => Renderbuffer -> Int -> RenderbufferInternalFormat -> Int -> Int -> m ()
renderbufferStorageMultisample (Renderbuffer n) samples fmt wid hei = glRenderbufferStorageMultisample n (fromIntegral samples) (marshalSizedFormat fmt) (fromIntegral wid) (fromIntegral hei)

renderbufferStorage :: MonadIO m => Renderbuffer -> RenderbufferInternalFormat -> Int -> Int -> m ()
renderbufferStorage (Renderbuffer n) fmt wid hei = glRenderbufferStorage n (marshalSizedFormat fmt) (fromIntegral wid) (fromIntegral hei)

---

data FramebufferAttachment
  = ColorAttachment0
  | ColorAttachment1
  | ColorAttachment2
  | ColorAttachment3
  | ColorAttachment4
  | ColorAttachment5
  | ColorAttachment6
  | ColorAttachment7
  | ColorAttachment8
  | ColorAttachment9
  | ColorAttachment10
  | ColorAttachment11
  | ColorAttachment12
  | ColorAttachment13
  | ColorAttachment14
  | ColorAttachment15
  | ColorAttachment16
  | ColorAttachment17
  | ColorAttachment18
  | ColorAttachment19
  | ColorAttachment20
  | ColorAttachment21
  | ColorAttachment22
  | ColorAttachment23
  | ColorAttachment24
  | ColorAttachment25
  | ColorAttachment26
  | ColorAttachment27
  | ColorAttachment28
  | ColorAttachment29
  | ColorAttachment30
  | ColorAttachment31
  | DepthAttachment
  | StencilAttachment
  | DepthStencilAttachment
  deriving (Eq, Ord, Show)

marshalFramebufferAttachment :: FramebufferAttachment -> GLenum
marshalFramebufferAttachment = \case
  ColorAttachment0       -> GL_COLOR_ATTACHMENT0
  ColorAttachment1       -> GL_COLOR_ATTACHMENT1
  ColorAttachment2       -> GL_COLOR_ATTACHMENT2
  ColorAttachment3       -> GL_COLOR_ATTACHMENT3
  ColorAttachment4       -> GL_COLOR_ATTACHMENT4
  ColorAttachment5       -> GL_COLOR_ATTACHMENT5
  ColorAttachment6       -> GL_COLOR_ATTACHMENT6
  ColorAttachment7       -> GL_COLOR_ATTACHMENT7
  ColorAttachment8       -> GL_COLOR_ATTACHMENT8
  ColorAttachment9       -> GL_COLOR_ATTACHMENT9
  ColorAttachment10      -> GL_COLOR_ATTACHMENT10
  ColorAttachment11      -> GL_COLOR_ATTACHMENT11
  ColorAttachment12      -> GL_COLOR_ATTACHMENT12
  ColorAttachment13      -> GL_COLOR_ATTACHMENT13
  ColorAttachment14      -> GL_COLOR_ATTACHMENT14
  ColorAttachment15      -> GL_COLOR_ATTACHMENT15
  ColorAttachment16      -> GL_COLOR_ATTACHMENT16
  ColorAttachment17      -> GL_COLOR_ATTACHMENT17
  ColorAttachment18      -> GL_COLOR_ATTACHMENT18
  ColorAttachment19      -> GL_COLOR_ATTACHMENT19
  ColorAttachment20      -> GL_COLOR_ATTACHMENT20
  ColorAttachment21      -> GL_COLOR_ATTACHMENT21
  ColorAttachment22      -> GL_COLOR_ATTACHMENT22
  ColorAttachment23      -> GL_COLOR_ATTACHMENT23
  ColorAttachment24      -> GL_COLOR_ATTACHMENT24
  ColorAttachment25      -> GL_COLOR_ATTACHMENT25
  ColorAttachment26      -> GL_COLOR_ATTACHMENT26
  ColorAttachment27      -> GL_COLOR_ATTACHMENT27
  ColorAttachment28      -> GL_COLOR_ATTACHMENT28
  ColorAttachment29      -> GL_COLOR_ATTACHMENT29
  ColorAttachment30      -> GL_COLOR_ATTACHMENT30
  ColorAttachment31      -> GL_COLOR_ATTACHMENT31
  DepthAttachment        -> GL_DEPTH_ATTACHMENT
  StencilAttachment      -> GL_STENCIL_ATTACHMENT
  DepthStencilAttachment -> GL_DEPTH_STENCIL_ATTACHMENT

newtype RenderbufferAttachPoint = RenderbufferAttachPoint FramebufferAttachment
  deriving (Eq, Ord, Show)

instance ForeignWrite RenderbufferAttachPoint Framebuffer (Maybe Renderbuffer) where
  writeR_ (RenderbufferAttachPoint m) t@(Framebuffer n) = \case
    Nothing -> glNamedFramebufferRenderbuffer n (marshalFramebufferAttachment m) GL_RENDERBUFFER 0 >> return t
    Just (Renderbuffer p) -> glNamedFramebufferRenderbuffer n (marshalFramebufferAttachment m) GL_RENDERBUFFER p >> return t

type MipmapLevel  = Int
type TextureLayer = Int

data TextureAttachPoint t = TextureAttachPoint FramebufferAttachment MipmapLevel
  deriving (Eq, Ord, Show)

instance TextureTarget t => ForeignWrite (TextureAttachPoint t) Framebuffer (Maybe (TextureObject t)) where
  writeR_ (TextureAttachPoint m i) t@(Framebuffer n) = \case
    Nothing -> glNamedFramebufferTexture n (marshalFramebufferAttachment m) 0 (fromIntegral i) >> return t
    Just (TextureObject p) -> glNamedFramebufferTexture n (marshalFramebufferAttachment m) p (fromIntegral i) >> return t

data TextureLayerAttachPoint t = TextureLayerAttachPoint FramebufferAttachment MipmapLevel TextureLayer
  deriving (Eq, Ord, Show)

instance TextureTarget t => ForeignWrite (TextureLayerAttachPoint t) Framebuffer (Maybe (TextureObject t)) where
  writeR_ (TextureLayerAttachPoint m i l) t@(Framebuffer n) = \case
    Nothing -> glNamedFramebufferTextureLayer n (marshalFramebufferAttachment m) 0 (fromIntegral i) (fromIntegral l) >> return t
    Just (TextureObject p) -> glNamedFramebufferTextureLayer n (marshalFramebufferAttachment m) p (fromIntegral i) (fromIntegral l) >> return t

newtype FramebufferStatus = FramebufferStatus FramebufferTarget
  deriving (Eq, Ord, Show)

data FramebufferStatusError
  = FramebufferUndefined
  | FramebufferIncompleteAttachment
  | FramebufferIncompleteMissingAttachment
  | FramebufferUnsupported
  | FramebufferIncompleteMultisample
  | FramebufferIncompleteLayerTargets
  | FramebufferCheckStatusError
  | FramebufferUnknownStatus -- This one's mine
  deriving (Eq, Ord, Show)

unmarshalFramebufferStatusError :: GLenum -> Maybe FramebufferStatusError
unmarshalFramebufferStatusError = \case
  GL_FRAMEBUFFER_COMPLETE                      -> Nothing
  GL_FRAMEBUFFER_UNDEFINED                     -> Just FramebufferUndefined
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT         -> Just FramebufferIncompleteAttachment
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT -> Just FramebufferIncompleteMissingAttachment
  GL_FRAMEBUFFER_UNSUPPORTED                   -> Just FramebufferUnsupported
  GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE        -> Just FramebufferIncompleteMultisample
  GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS      -> Just FramebufferIncompleteLayerTargets
  0                                            -> Just FramebufferCheckStatusError
  _                                            -> Just FramebufferUnknownStatus

data FramebufferTarget
  = FramebufferTarget
  | FramebufferTargetDraw
  | FramebufferTargetRead
  deriving (Eq, Ord, Show)

marshalFramebufferTarget :: FramebufferTarget -> GLenum
marshalFramebufferTarget = \case
  FramebufferTarget -> GL_FRAMEBUFFER
  FramebufferTargetDraw -> GL_DRAW_FRAMEBUFFER
  FramebufferTargetRead -> GL_READ_FRAMEBUFFER

instance ForeignRead FramebufferStatus Framebuffer (Maybe FramebufferStatusError) where
  readR_ (FramebufferStatus t) (Framebuffer n) = unmarshalFramebufferStatusError <$> glCheckNamedFramebufferStatus n (marshalFramebufferTarget t)
