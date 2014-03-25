{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad         (unless, (>=>))
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import           System.Environment    (getArgs)
import           System.IO             (BufferMode (..), hSetBuffering, isEOF,
                                        stdout)
import           Text.Printf           (printf)

type LineNumber = Int
type Line = ByteString
type Lines = [ByteString]
type Output = ByteString
type Formatter = LineNumber -> Line -> (LineNumber, Line)

-- | Given a formatting function and a list of lines, format each line
-- according to that function and return the lines joined together into
-- a single string.
number :: Formatter
       -> Lines
       -> Output
number f = number' f "" 1

number' :: Formatter
        -> Output
        -> LineNumber
        -> Lines
        -> Output
number' _ s _    []  = s
number' f s n (x:xs) = let (m, y) = f n x
                       in number' f (BS.concat [s, y]) m xs

-- | Turns an integer into a string that can be used to "number" a line.
formatNumber :: Int -> ByteString
formatNumber = C.pack . printf "%6d  "

-- | A line number formatter that increments the counter on every line.
standardNumber :: Formatter
standardNumber n x = (n + 1, BS.concat [formatNumber n, x, "\n"])

-- | A line number formatter that only increments the counter on
-- non-blank lines.
nonBlankNumber :: Formatter
nonBlankNumber n "" = (n, "\n")
nonBlankNumber n x  = (n + 1, BS.concat [formatNumber n, x, "\n"])

-- | Reads standard in and writes it to standard out.
catStdin :: IO ()
catStdin = interact id

-- | Reads lines from standard in and prints them out after formatting
-- them according to the given formatting function.
catStdinFormatted :: Formatter -> IO ()
catStdinFormatted f = catStdinFormatted' f 1

catStdinFormatted' :: Formatter -> LineNumber -> IO ()
catStdinFormatted' f n =
  do done <- isEOF
     unless done $
       do x <- BS.getLine
          let (m, y) = f n x
          BS.putStr y
          catStdinFormatted' f m

-- | Reads a file and prints it to standard out.
catFile :: FilePath -> IO ()
catFile = readFile >=> putStr

-- | Reads a file and prints it to standard out after formatting each
-- line according to the given formatting function.
catFileFormatted :: Formatter -> FilePath -> IO ()
catFileFormatted f = BS.readFile >=> return . number f . C.split '\n' >=> BS.putStr

cat :: [String] -> IO ()
cat    []  = catStdin
cat (x:xs) = if length x >= 2 && head x == '-'
                then mapM_ (formattedCat x) xs
                else mapM_ simpleCat (x:xs)
  where simpleCat "-" = catStdin
        simpleCat  p  = catFile p

        formattedCat p
          | 'b' `elem` p = formattedCat' nonBlankNumber
          | 'n' `elem` p = formattedCat' standardNumber
          | otherwise    = simpleCat

        formattedCat' f "-" = catStdinFormatted f
        formattedCat' f  p  = catFileFormatted f p

main :: IO ()
main = hSetBuffering stdout (BlockBuffering Nothing) >> getArgs >>= cat
