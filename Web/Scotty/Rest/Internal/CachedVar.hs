{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Web.Scotty.Rest.Internal.CachedVar
  (
    CachedVar
  , newEmtpyCachedVar
  , readCachedVar
  , writeCachedVar
  , writeAndReturnCachedVar
  ) where

import BasePrelude

type CachedVar a = IORef (Maybe a)

newEmtpyCachedVar :: IO (CachedVar a)
newEmtpyCachedVar = newIORef Nothing

writeCachedVar :: CachedVar a -> a -> IO ()
writeCachedVar var value = writeIORef var (Just value)

writeAndReturnCachedVar :: CachedVar a -> a -> IO a
writeAndReturnCachedVar var value = writeCachedVar var value >> return value

readCachedVar :: CachedVar a -> IO a
readCachedVar var = fromJust <$> readIORef var
