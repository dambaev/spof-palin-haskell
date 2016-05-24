{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ForeignFunctionInterface#-}
{-#LANGUAGE BangPatterns #-}
module Main where

import Debug.Trace

import Data.Char
    ( chr
    , ord
    )
import Data.Bits as B
    ( shiftR
    )
import Prelude  hiding 
    ( take
    , drop
    , getLine
    , length
    , putStrLn
    )
import Control.Monad
    ( when
    )
import qualified Data.List  as DL
    ( take
    )

import Data.ByteString.Builder as BB
    ( hPutBuilder
    , 
    )
import Data.ByteString as BS
    ( append
    )
import Data.ByteString.Char8 as C8
    ( readInt
    , getLine
    , append
    , putStrLn
    , length
    , take
    , drop
    , ByteString(..)
    , pack
    , index
    , reverse
    , null
    , empty
    , getContents
    , lines
    , hPutStrLn
    , putStr
    )
import System.IO
    ( stdout
    , hFlush
    , hSetBuffering
    , BufferMode(..)
    )
import Data.Maybe
import Foreign.Marshal.Alloc
import Foreign.ForeignPtr
import Foreign.Ptr
import Data.ByteString.Internal
import Foreign.C.Types
import Foreign.Storable
import Data.Word

main :: IO ()
main = do
    mcount <- C8.getLine >>= return . readInt
    buf::(ForeignPtr Word8) <- mallocForeignPtrBytes 1000001
    
    case mcount of
        Nothing -> return ()
        Just (count,_ ) -> readLines count buf
    return ()



readLines:: Int -> ForeignPtr Word8 -> IO ()
readLines 0 _ = return ()
readLines !count ptr = do
    (left,center) <- getLine >>= getNextPalyndrome ptr 
    C8.putStr left >> C8.putStr center >> C8.putStrLn ( C8.reverse left)
    readLines (count-1) ptr

getNextPalyndrome:: ForeignPtr Word8-> C8.ByteString-> IO (C8.ByteString, C8.ByteString)
getNextPalyndrome ptr line =  do
    let line_len = C8.length line
        half = fromIntegral $ line_len `shiftR` 1
        left = C8.take half line
        rleft = C8.reverse left
        left_len = half
        center_len = line_len - half * 2
        right = drop (half+center_len) line
        right_len = half
        center | center_len == 0 = ""
               | otherwise = take 1 $ drop half line 
        left_center = C8.take (left_len + center_len) line
        left_center_len = C8.length left_center
--        next_left_center = incString $ left_center
    putStrLn "incString"
    next_left_center <- incString left_center ptr

    let next_left_center_len = C8.length next_left_center
        (next_left, next_center) = if next_left_center_len == (left_len + center_len)
            then (take left_len next_left_center, take center_len $ drop left_len next_left_center)
            else if center_len == 0
                then (take left_len next_left_center, take 1 $ drop left_len next_left_center)
                else (take (left_len + 1) next_left_center, "")
        right_base = half+center_len
        isRLeftMoreRight = isMoreRec 0
        isMoreRec !currid | currid == half = False
        isMoreRec !currid = 
            let !lc = C8.index line (half - 1 - currid)
                !rc = C8.index line (right_base+currid)
            in case 1 of
                _ | lc > rc -> True
                _ | lc < rc -> False
                _ -> isMoreRec (currid+1)
    next_left_center <- incString left_center ptr
    case line_len of
        _ | line_len < 2 || (line_len == 2 && line == "10") -> return ("1", C8.empty)
        _ | isRLeftMoreRight -> return (left, center)
        _ -> return (next_left, next_center)

incString:: C8.ByteString -> ForeignPtr Word8-> IO C8.ByteString
incString !string@(PS srcFPtr srcOffset src_len) !buf = do
    (!offset,!len) <- do 
        withForeignPtr srcFPtr $ \srcPtrBase -> withForeignPtr buf $ \ptrBuf -> do
            let !srcPtr = plusPtr srcPtrBase srcOffset
                !tailedPtr = plusPtr ptrBuf 1
            memcpy tailedPtr srcPtr src_len
            let incChar:: Char-> Char
                incChar !x = chr $ (ord x) + 1
                inc' !id | id == 0 = do
                    C8.putStrLn "all done, add 1"
                    pokeByteOff ptrBuf 0 '2' 
                    return (0, src_len+1)
                inc' !id | C8.index string (id-1) == '9' = do
                    C8.putStrLn "still 9 "
                    pokeByteOff tailedPtr (id-1) '0'
                    inc' (id-1) 
                inc' !id = do
                    !oldchar <- peekByteOff tailedPtr (id-1)
                    pokeByteOff tailedPtr (id-1) $! incChar $! oldchar
                    return (1,src_len)
            return $ inc' src_len
    C8.putStrLn $ "was = " `C8.append` string
    C8.putStrLn $ "offset,len = " `C8.append` (pack $ show srcOffset) `C8.append` " " `C8.append` (pack $ show src_len)
    return $! (PS buf offset len)





--foreign import ccall unsafe "string.h" memcpy  :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)
--foreign import ccall unsafe "string.h" memmove :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)
--foreign import ccall unsafe "string.h" memset  :: Ptr a -> CInt  -> CSize -> IO (Ptr a)
