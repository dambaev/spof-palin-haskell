{-#LANGUAGE BangPatterns #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE OverloadedStrings #-}
module Main where

import Debug.Trace

import Data.Char
    ( chr
    , ord
    )

import Prelude  hiding 
    ( take
    , drop
    , getLine
    , length
    , putStrLn
    )

import Data.ByteString.Builder as BB
    ( hPutBuilder
    )
import Data.ByteString as BS
    ( getLine
    , append
    )
import Data.ByteString.Char8 as C8
    ( readInt
    , readInteger
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
    )

import Data.Maybe

main :: IO ()
main = do
    mcount <- getLine >>= return . C8.readInt 
    case mcount of
        Nothing -> return ()
        Just (!count,_ ) -> readLines count 
    return ()



readLines:: Int -> IO ()
readLines 0 = return ()
readLines !count = do
    !(line) <- getLine
    putStrLn $! getNextPalyndrome line
    readLines (count - 1)


getNextPalyndrome:: C8.ByteString-> C8.ByteString
getNextPalyndrome line = 
    let !line_len =  fromIntegral $! C8.length line
        half = fromIntegral $! line_len `div` 2
        left = C8.take half line
        rleft = C8.reverse left
        left_len = C8.length left
        center_len = line_len `mod` 2
        right = drop (half+center_len) line
        right_len = C8.length right
        center | center_len == 0 = ""
               | otherwise = take 1 $ drop half line 
        left_center = left `BS.append` center
        left_center_len = C8.length left_center
        next_left_center = incString $! left_center
        next_left_center_len = C8.length next_left_center

        (!next_left, !next_center) = if next_left_center_len == (left_len + center_len)
            then (take left_len next_left_center, take center_len $! drop left_len next_left_center)
            else if center_len == 0
                then (take left_len next_left_center, take 1 $ drop left_len next_left_center)
                else (take (left_len + 1) next_left_center, "")
        right_base = half+center_len
        isRLeftMoreRight = isMoreRec 0
        isMoreRec:: Int -> Bool
        isMoreRec !currid | currid == half = False
        isMoreRec !currid = 
            let lc = C8.index line (half - 1 - currid)
                rc = C8.index line (half+center_len+currid)
            in case 1 of
                _ | lc > rc -> True
                _ | lc < rc -> False
                _ -> isMoreRec (currid+1)
    in
    case line_len of
        _ | line_len < 2 || (line_len == 2 && line == "10") -> "11"
        _ | isRLeftMoreRight -> left `BS.append` center `BS.append` rleft
        _ -> next_left `BS.append` next_center `BS.append` (C8.reverse next_left)

incString:: C8.ByteString -> C8.ByteString
incString string | C8.null string = string
incString string = 
    let !src_len = C8.length string
        incChar:: Char-> Char
        incChar x = chr $! (ord x) + 1
        inc' !string !id !acc | id == 0 = C8.pack ('1':acc)
        inc' !string !id !acc | C8.index string (id-1) == '9' = inc' string (id-1) ('0':acc)
        inc' !string !id !acc = (C8.take (id-1) string) `BS.append` (C8.pack ((incChar $! C8.index string (id-1)):acc))
   in  inc' string src_len []






