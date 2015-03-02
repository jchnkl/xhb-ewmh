{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}

module Graphics.XHB.Ewmh.Serialize (Serialize(..)) where

import Control.Monad (replicateM_)
import Data.Binary.Get
import Data.Binary.Put
import Data.Char (chr, ord)
import Data.List (intersperse)
import Data.Word (Word8, Word32)
import Graphics.XHB
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.DList as DL

putSkip8 :: Int -> Put
putSkip8 n = replicateM_ n $ putWord8 0

putSkip16 :: Int -> Put
putSkip16 n = replicateM_ n $ putWord16host 0

putSkip32 :: Int -> Put
putSkip32 n = replicateM_ n $ putWord32host 0

class Serialize a where
    serialize :: a -> Put

    toBytes :: a -> [Word8]
    toBytes = B.unpack . runPut . serialize

    deserialize :: Get a

    fromBytes :: [Word8] -> Either String a
    fromBytes bs = case runGetOrFail deserialize (B.pack bs) of
        Right (_, _, a) -> Right a
        Left  (_, _, e) -> Left e

    serializeList :: [a] -> Put
    serializeList = mapM_ serialize

    deserializeList :: Get [a]
    deserializeList = fmap DL.toList $ loop DL.empty
        where
        loop as = do
            b <- isEmpty
            if b then return as
                 else deserialize >>= loop . DL.snoc as

instance Serialize a => Serialize [a] where
    serialize = serializeList
    deserialize = deserializeList

instance (Serialize a, Serialize b) => Serialize (a, b) where
    serialize (a,b) = serialize a >> serialize b
    deserialize = do
        a <- deserialize
        b <- deserialize
        return (a, b)

instance (Serialize a, Serialize b, Serialize c) => Serialize (a, b, c) where
    serialize (a,b,c) = serialize a >> serialize b >> serialize c
    deserialize = do
        a <- deserialize
        b <- deserialize
        c <- deserialize
        return (a, b, c)

instance (Serialize a, Serialize b, Serialize c, Serialize d) => Serialize (a, b, c, d) where
    serialize (a,b,c,d) = serialize a >> serialize b >> serialize c >> serialize d
    deserialize = do
        a <- deserialize
        b <- deserialize
        c <- deserialize
        d <- deserialize
        return (a, b, c, d)

instance Serialize Char where
    serialize = putWord8 . fromIntegral . ord
    deserialize = fmap (chr . fromIntegral) getWord8

instance Serialize String where
    serialize = mapM_ serialize

    deserialize = fmap C.unpack getRemainingLazyByteString

    serializeList = mapM_ putWord8 . map (fromIntegral . ord) . concat . intersperse "\0"

    deserializeList = fmap convert getRemainingLazyByteString
        where nul      = fromIntegral . ord $ '\0'
              convert  = map C.unpack . B.splitWith (== nul)

instance Serialize Word8 where
    serialize = putWord8
    deserialize = getWord8

instance Serialize Word32 where
    serialize = putWord32host
    deserialize = getWord32host

instance Serialize Int where
    -- this might cause some breakage, but practically `#define`s are not < 0
    serialize = putWord32host . fromIntegral
    deserialize = fmap fromIntegral getWord32host

instance Serialize ATOM where
    serialize   = putWord32host . fromXid . toXid
    deserialize = fmap (fromXid . toXid) getWord32host

instance Serialize WINDOW where
    serialize   = putWord32host . fromXid . toXid
    deserialize = fmap (fromXid . toXid) getWord32host

instance Serialize ClientMessageEvent where
    serialize (MkClientMessageEvent fmt win typ dat) = do
        putWord8 33 -- 33 ^= ClientMessageEvent
        putWord8 fmt
        putSkip8 2
        serialize win
        serialize typ
        serialize dat

    deserialize = error "deserialize for ClientMessageEvent not implemented"

instance Serialize ClientMessageData where
    serialize (ClientData8  ws) = do mapM_ putWord8 ws
                                     putSkip8 (20 - length ws)
    serialize (ClientData16 ws) = do mapM_ putWord16host ws
                                     putSkip16 (10 - length ws)
    serialize (ClientData32 ws) = do mapM_ putWord32host ws
                                     putSkip32 (5 - length ws)

    deserialize = error "deserialize for ClientMessageData not implemented"
