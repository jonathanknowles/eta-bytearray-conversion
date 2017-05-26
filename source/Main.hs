module Main where

import Data.ByteString (ByteString)
import Java
import System.IO (BufferMode (..), hSetBuffering, stdout)

import qualified Data.ByteString as B

createJByteArray :: Int -> JByteArray
createJByteArray i = pureJava $ anew i

toByteString :: JByteArray -> ByteString
toByteString ba = B.pack $ fromIntegral <$> bs
    where bs = fromJava ba :: [Byte]

main :: IO ()
main = do
        hSetBuffering stdout NoBuffering
        mapM_ test byteArraySizes
    where
        byteArraySizes = [2 ^ x | x <- [0 .. 24]]
        test :: Int -> IO ()
        test i = do
            putStr "Testing with array of size (bytes): "
            print i
            let j = B.length $ toByteString $ createJByteArray i
            putStr "Final converted array size (bytes): "
            print j

