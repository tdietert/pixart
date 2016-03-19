module Main where

import System.Environment
import System.Directory

import EditPics
import Codec.Picture (readPng, savePngImage, convertRGBA8)
import Control.Monad
import qualified Data.ByteString as BS

main :: IO ()
main = do
    fp <- getArgs
    case fp of
      [filepath] -> do
        isFile <- doesFileExist filepath
        guard isFile
        putStrLn "Editing photo..."
        png <- readPng filepath
        case editPixelsPng =<< png of
          Left err -> putStrLn err
          Right resPng -> do
            putStrLn "Editing successful."
            savePngImage filepath resPng
      otherwise -> putStrLn "usage: ./pixart <validFilePath>"

