module Graphics.Binding.OpenGL.Synchro where

import Control.Monad.IO.Class
import Data.Word
import Foreign.Ptr
import Graphics.GL.Core45
import Graphics.GL.Types

newtype GLFence = GLFence
  { _getGLFencePtr :: GLsync
  } deriving (Eq, Ord)

instance Show GLFence where
  show _ = "GLFence"

when :: Applicative f => Bool -> f () -> f ()
when True f  = f
when False _ = pure ()

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM x f = do
  t <- x
  when t f

deleteGLFence :: MonadIO m => GLFence -> m ()
deleteGLFence (GLFence sync) = when (nullPtr /= sync) (glDeleteSync sync)

lockGLFence :: MonadIO m => GLFence -> m GLFence
lockGLFence s = deleteGLFence s >> GLFence <$> glFenceSync GL_SYNC_GPU_COMMANDS_COMPLETE 0

waitGLFence :: MonadIO m => Word64 -> GLFence -> m ()
waitGLFence n (GLFence sync) = when (nullPtr /= sync) go
  where
    cond x = x == GL_ALREADY_SIGNALED || x == GL_CONDITION_SATISFIED
    go = unlessM (cond <$> glClientWaitSync sync GL_SYNC_FLUSH_COMMANDS_BIT n) go
