module Main where

import System.IO

maxGuesses = 6
wordLength = 5

debugLog :: String -> IO ()
debugLog s =
  let debug = False in
  if debug then putStrLn s else return ()

noRepeatLetters :: String -> Bool
noRepeatLetters word =
  undefined

--------------------------------------------------------------------------------
-- Model and Actions

data Model =
  Model () -- TODO: fill this in

data Action =
  KeyPress Char

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  undefined

--------------------------------------------------------------------------------
-- Controller

controller :: Model -> IO Model
controller =
  undefined

update :: Action -> Model -> Model
update =
  undefined

--------------------------------------------------------------------------------
-- View

view :: Model -> IO ()
view =
  undefined

mapIO_ :: (a -> IO b) -> [a] -> IO ()
mapIO_ f as =
  undefined
