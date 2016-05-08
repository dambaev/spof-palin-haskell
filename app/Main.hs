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

import Data.ByteString as BS
    ( getLine
    , append
    )
import Data.ByteString.Char8 as C8
    ( readInt
    , putStrLn
    , length
    , take
    , drop
    , ByteString(..)
    , pack
    , index
    , reverse
    , null
    )

import Data.Maybe

main :: IO ()
main = do
    mcount <- getLine >>= return . readInt
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
    let !line_len = C8.length line
        half = line_len `div` 2
        left = take half line
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

        next_right = incString $ right
        next_right_len = C8.length next_right
        next_left = case 1 of
            _ | next_left_center_len < 1 ->  ""
            _ | (next_left_center_len > left_center_len && center_len == 0)
                || (next_left_center_len == left_center_len && center_len == 1) ->  C8.take (next_left_center_len - 1) next_left_center
            _ | otherwise -> next_left_center
        next_center = case 1 of
            _ | next_left_center_len < 1 -> ""
            _ | (next_left_center_len > left_center_len && center_len == 0) 
                || (next_left_center_len == left_center_len && center_len == 1)  -> C8.drop (next_left_center_len - 1 ) next_left_center
            _ | otherwise -> ""
    in
    case line_len of
        _ | line_len < 2 || line == "10" -> "11"
        _ | next_right <= rleft && next_right_len <= left_len -> left `BS.append` center `BS.append` rleft
        _ -> next_left `BS.append` next_center `BS.append` (C8.reverse next_left)

incString:: C8.ByteString -> C8.ByteString
incString string | C8.null string = string
incString string = 
    let !src_len = C8.length string
        incChar:: Char-> Char
        incChar x = chr $! (ord x) + 1
        inc' !string !id !acc | id >= src_len = C8.pack ('1':acc)
        inc' !string !id !acc | C8.index string id == '9' = inc' string (id+1) ('0':acc)
        inc' !string !id !acc = (C8.reverse $! C8.drop (id+1) string) `BS.append` (C8.pack ((incChar $! C8.index string id):acc))

   in  inc' (C8.reverse string) 0 []







