{-#LANGUAGE BangPatterns #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE OverloadedStrings #-}
module Main where

import Debug.Trace

import Data.Char
    ( chr
    , ord
    )
import Data.Bits as B
    ( shiftR
    )
import Prelude as P hiding 
    ( take
    , drop
    , getLine
    , length
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

main :: IO ()
main = do
    mcount <- C8.getLine >>= return . readInt
    case mcount of
        Nothing -> return ()
        Just (count,_ ) -> readLines count 
    return ()



readLines:: Int -> IO ()
readLines 0 = return ()
readLines !count = do
    (left_center, left_len, center_len) <- getLine >>= return . getNextPalyndrome 
    C8.putStr ( C8.take (left_len + center_len) left_center) >> C8.putStrLn ( C8.reverse $ C8.take left_len left_center)
    readLines (count-1)

getNextPalyndrome:: C8.ByteString-> (C8.ByteString, Int, Int)
getNextPalyndrome line = 
    let line_len = C8.length line
        half = fromIntegral $ line_len `shiftR` 1
        left = C8.take half line
        center_len = line_len - half * 2
        right = drop (half+center_len) line
        left_center = C8.take (half + center_len) line
        left_center_len = half + center_len
        next_left_center = incString $ left_center
        next_left_center_len = C8.length next_left_center

        (next_left_len, next_center_len) = case 1 of
            _ | next_left_center_len > (half + center_len) -> if  center_len == 1
                    then (half + 1, 0)
                    else (half, 1)
            _ -> (half, center_len)
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
    in
    case line_len of
        _ | line_len < 2 || (line_len == 2 && line == "10") -> ("11", 0, 0)
        _ | isRLeftMoreRight -> (left_center, half, center_len)
        _ -> (next_left_center, next_left_len, next_center_len)

incString:: C8.ByteString -> C8.ByteString
incString !string | C8.null string = string
incString !string = 
    let src_len = C8.length string
        incChar:: Char-> Char
        incChar !x = chr $ (ord x) + 1
        inc' string !id !acc | id == 0 = C8.pack ('1':acc)
        inc' string !id !acc | C8.index string (id-1) == '9' = inc' string (id-1) ('0':acc)
        inc' string !id !acc = (C8.take (id-1) string) `BS.append` (C8.pack $! ((incChar $! C8.index string (id-1)):acc))
   in  inc' string src_len []






