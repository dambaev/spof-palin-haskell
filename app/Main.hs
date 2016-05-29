{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ForeignFunctionInterface#-}
{-#LANGUAGE BangPatterns #-}
module Main where

import Debug.Trace

import System.IO
    ( hTell
    , stdin
    , hGetBufSome
    , hPutBuf
    )
import qualified GHC.IO.FD as FD
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
    , unpack
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
import Foreign.C.String
import Foreign.Storable
import Data.Word
import Foreign.ForeignPtr.Unsafe

readBufSz = 500 * 1000001

line_sz = 1000002

main :: IO ()
main = do
--    mcount <- C8.getLine >>= return . readInt
    ret_buf::(ForeignPtr Word8) <- mallocForeignPtrBytes line_sz
    let ret_buf_bs = PS ret_buf 0 line_sz
    read_buf::(ForeignPtr Word8) <- mallocForeignPtrBytes line_sz
    let read_buf_bs =  PS read_buf 0 line_sz

    fin <- fdopen 0 "r"
    fout <- fdopen 1 "w"

    !line <- fgets ret_buf_bs fin
    let mcount = readInt line
    
    let !newstate = ((PS read_buf 0 readBufSz), 0, 0)


    case mcount of
        Nothing -> return ()
        Just (count,_ ) -> readLines count fin ret_buf_bs read_buf_bs fout
    return ()



readLines:: Int -> Ptr CFile -> ByteString -> ByteString-> Ptr CFile-> IO ()
readLines 0 _ _ _  _= return ()
readLines !count !fin !ret_buf !read_buf !fout = do
    --(!line, !newstate) <- {-# SCC bufread #-} bufferedGetLine state  
    !line <- fgets read_buf fin 
    {-# SCC calc_plindrome #-} getNextPalyndrome ret_buf line >>= fwrite fout
    readLines (count-1) fin ret_buf read_buf fout

customReverse:: ByteString-> Int-> Int-> ByteString -> IO ByteString
customReverse srcBS@(PS !buf srcOffset srcLen) left_len center_len (PS !dstBuf dstOffset dstLen) =
    withForeignPtr buf $ \p_fsrc -> withForeignPtr dstBuf $ \p_fdst -> do
    let p_src::(Ptr Word8) = plusPtr p_fsrc srcOffset
        !p_dst = plusPtr p_fdst dstOffset
        !right_base = left_len + center_len
        !newlen = right_base + left_len
    memcpy p_dst p_src right_base
    let rev' 0 = return ()
        rev' !idx = do
            peekElemOff p_src (idx - 1) >>= pokeElemOff p_dst ( newlen  - idx)
            rev' (idx-1)
    rev' left_len
    return $! PS dstBuf dstOffset $! newlen

getNextPalyndrome:: ByteString -> C8.ByteString-> IO C8.ByteString
getNextPalyndrome ptr@(PS buf offset len) line =  do
    let !line_len = {-# SCC line_len #-} C8.length line
        !half = {-# SCC half #-} fromIntegral $ line_len `shiftR` 1
        !left = {-# SCC left #-} C8.take half line
        !center_len = {-# SCC center_len #-} line_len - half * 2
        !right = {-# SCC right #-}drop left_center_len line
        !left_center = {-# SCC left_center #-} C8.take left_center_len line
        !left_center_len = {-# SCC left_center_len #-}half + center_len

        isRLeftMoreRight = {-# SCC isRLeftMoreRight #-} isMoreRec 0
        isMoreRec !currid | currid == half = False
        isMoreRec !currid = 
            let !lc = C8.index line (half - 1 - currid)
                !rc = C8.index line (left_center_len+currid)
            in case 1 of
                _ | lc > rc -> True
                _ | lc < rc -> False
                _ -> isMoreRec (currid+1)
    case 1 of
        _ | line_len < 2 || (line_len == 2 && line == "10") -> withForeignPtr buf $ \p_buf-> do
            pokeElemOff p_buf 0 $ fromIntegral $ ord '1'
            pokeElemOff p_buf 1 $ fromIntegral $ ord '1'
            return $! PS buf 0 2 
        _ | isRLeftMoreRight -> {-# SCC customReverse1 #-} customReverse left_center half center_len ptr
        _ -> do
            !next_left_center <-  {-# SCC incString_call #-} incString left_center ptr
            let !next_left_center_len = {-# SCC next_left_center_len #-} C8.length next_left_center
                (!next_left_len, !next_center_len) = {-# SCC next_center_len #-} case 1 of
                    _ | next_left_center_len > (half + center_len) -> if  center_len == 1
                            then (half + 1, 0)
                            else (half, 1)
                    _ -> (half, center_len)

            {-# SCC customReverse2 #-} customReverse next_left_center next_left_len next_center_len line

incString:: C8.ByteString -> ByteString-> IO C8.ByteString
incString !string@(PS srcFPtr srcOffset src_len) (PS !buf _ _) = withForeignPtr srcFPtr $ \srcPtrBase -> withForeignPtr buf $ \ptrBuf -> do
    let !srcPtr = plusPtr srcPtrBase srcOffset
        (!tailedPtr)::Ptr Word8 = plusPtr ptrBuf 1
        !buf_len = if src_len > 1000000 then 1000000
            else src_len
    memcpy tailedPtr srcPtr buf_len
    let inc' 0 = do
            poke ptrBuf $! fromIntegral $ ord '1' 
            return (0, src_len+1)
        inc' !id = do
            oldchar <- peekElemOff tailedPtr (id-1)
            case chr $ fromIntegral oldchar of
                '9' -> do
                    pokeElemOff tailedPtr (id-1) $! fromIntegral $! ord '0'
                    inc' (id-1) 
                _ -> do
                    pokeElemOff tailedPtr (id-1) $! oldchar + 1
                    return (1,src_len)
    (!offset,!len) <- inc' buf_len
    

    return $! (PS buf offset len)

foreign import ccall "string.h memchr" _c_memchr
    :: Ptr Word8-> CInt-> CSize-> IO (Ptr Word8)

_memchr:: Ptr Word8-> Word8-> Int-> IO (Maybe (Ptr Word8))
_memchr ptr char maxsize = do
    p_ret <- _c_memchr ptr (fromIntegral char) (fromIntegral maxsize)
    if p_ret == nullPtr
        then return Nothing
        else return (Just p_ret)



_memmove:: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
_memmove !p_dst !p_src 0 = return ()
_memmove !p_dst !p_src !sz = do
    peek p_src >>= poke p_dst
    _memmove (p_dst `plusPtr` 1) (p_src `plusPtr` 1) (sz - 1)

bufferedGetLine:: (ByteString, Int, Int) -> IO (ByteString, (ByteString,Int, Int))
bufferedGetLine (buffer@(PS !buf !bufOffset !bufLen), !nextDataOffset, !nextDataLen) 
    -- no buffer space
    | nextDataLen == bufLen && nextDataOffset == 0 = {-# SCC no_buffer_space_left #-} return (buffer, (buffer, 0, 0))
    -- no data in buffer
    | nextDataLen == 0 = withForeignPtr buf $ \p_buf -> {-# SCC no_data_in_buffer #-}do
        !readed <- hGetBufSome stdin p_buf bufLen
        if readed == 0 then return (buffer, (buffer, 0, 0))
            else bufferedGetLine (buffer, 0, readed)
    -- move offseted data to the begin
    | --   nextDataOffset >= (bufLen `shiftR` 1) ||
     (nextDataOffset > 0 && nextDataOffset + nextDataLen == bufLen ) = {-# SCC move_data_to_start #-} withForeignPtr buf $ \p_buf -> do
        let p_src = p_buf `plusPtr` nextDataOffset
        _memmove p_buf p_src nextDataLen
        bufferedGetLine (buffer, 0, nextDataLen)
    -- find next line and return it
    | otherwise = {-# SCC find_newline #-} withForeignPtr buf $ \p_obuf -> do
        let !p_buf = p_obuf `plusPtr` nextDataOffset
            !newline = fromIntegral $! ord '\n'
{-        (!mPos, !newNextDataLen) <- do
            !_mPtr <- _memchr p_buf newline nextDataLen
            case _mPtr of
                Just !some -> return (Just some, nextDataLen)
                Nothing-> {-# SCC reread_buffer #-} do
                    -- read to the end
                    !readed <- hGetBufSome stdin (p_buf `plusPtr` nextDataLen) (bufLen - nextDataLen - nextDataOffset)
                    case readed of
                        0 -> return (Nothing, nextDataLen)
                        -- find newline in new 
                        _ -> do
                            !ret <- _memchr (p_buf `plusPtr` nextDataLen) newline readed
                            return (ret, nextDataLen + readed)
        case mPos of
            -- read more data and retry
            Nothing-> {-# SCC readmore_retry #-} bufferedGetLine (buffer, nextDataOffset, newNextDataLen)
            Just !newlineptr -> do
                let !newlinepos = newlineptr `minusPtr` p_buf
                {-# SCC return_new_state#-} return $! ((PS buf nextDataOffset newlinepos), (buffer, nextDataOffset + newlinepos+1, newNextDataLen - newlinepos - 1))
-}
        !mPos <- _memchr p_buf newline nextDataLen
        case mPos of
            Nothing-> do
                !readed <- hGetBufSome stdin (p_buf `plusPtr` nextDataLen) (bufLen - nextDataLen - nextDataOffset)
                case readed of
                    0 -> return (PS buf nextDataOffset nextDataLen, (buffer, 0, 0))
                    _ -> bufferedGetLine (buffer, nextDataOffset, nextDataLen + readed)
            Just !ptr -> do
                let !newlinepos = ptr `minusPtr` p_buf
                return $! ((PS buf nextDataOffset newlinepos), (buffer, nextDataOffset + newlinepos+1, nextDataLen - newlinepos - 1))

foreign import ccall "stdio.h fdopen" c_fdopen
    :: CInt-> Ptr CChar-> IO (Ptr CFile)

fdopen:: Int-> String-> IO (Ptr CFile)
fdopen fd mode = withCString mode $ \p_str -> do
    c_fdopen (fromIntegral fd)  p_str

foreign import ccall "stdio.h fgets" c_fgets
    :: Ptr Word8-> CInt-> Ptr CFile-> IO (Ptr CChar)

fgets:: ByteString-> Ptr CFile-> IO ByteString
fgets (PS buf offset len) fd = withForeignPtr buf $ \p_buf-> do
    !ret <- c_fgets p_buf (fromIntegral $! len) fd
    if nullPtr == ret
        then return $! PS buf 0 0
        else do
            !len <- c_strlen (castPtr p_buf) >>= return . fromIntegral
            if len == 0
                then return $! PS buf 0 0
                else do
                    chr <- peek ((p_buf `plusPtr` (len-1)):: Ptr Word8)
                    if chr == (fromIntegral $ ord '\n')
                        then return $! PS buf 0 (len-1)
                        else return $! PS buf 0 len

foreign import ccall "stdio.h fwrite" c_fwrite
    :: Ptr Word8-> CInt-> CInt-> Ptr CFile-> IO Int

fwrite:: Ptr CFile-> ByteString-> IO Int
fwrite fout (PS !buf offset len) = withForeignPtr buf $ \p_buf -> do
    let p_dst = (p_buf `plusPtr` offset)::Ptr Word8
    pokeElemOff p_dst len $ fromIntegral $ ord '\n'
    c_fwrite p_dst 1 (fromIntegral $ len + 1 ) fout


hwrite:: ByteString-> IO ()
hwrite (PS buf offset len) = withForeignPtr buf $ \p_buf -> do
    hPutBuf stdout (p_buf `plusPtr` offset) len
