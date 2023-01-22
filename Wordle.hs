{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use zipWith" #-}
module Main where

import System.IO
import System.Exit
import System.Random
import System.Environment
import Data.List
import Data.Char
import Text.Read
import Data.Maybe

-- Programmed on Windows, which was... frustrating.
-- One character at a time doesn't really work properly in Windows terminals. 

data Model =
  WordleModel {
    modelWordList :: [String],
    modelAnswer :: String,
    modelBoard :: [String],
    modelCurrGuess :: String,
    modelGuessColor :: [(Char, String)],
    modelGuessesLeft :: Int,
    modelKeyboard :: [(Char, String)]
  }
  | EmptyBoard
  deriving Show

data Action =
  KeyPress { cVal :: Char }

----------------------

{-- Consulted https://hackage.haskell.org/package/ for functions including nub, 
isJust, and basically any others seen here we didn't learn about in class. --}

howToPlay = "\
\HOW TO PLAY\
\\
\Guess the WORDLE in 6 tries.\
\\
\Each guess must be a valid 5 letter word. Hit the enter button to submit.\
\\
\Examples\
\\
\ \x1b[48;5;82m W \x1b[0m\x1b[48;5;243m e a r y \x1b[0m  The letter W is in the word and in the correct spot.\
\ \x1b[48;5;243m p \x1b[0m\x1b[48;5;226m i l l s \x1b[0m  The letter I is in the word but in the wrong spot.\
\ \x1b[48;5;243m v a g u e \x1b[0m  None of the letters are in the word in any spot."

tooManyArgs = "Usage:\
\\
\  ./wordle                  Play random game\
\  ./wordle gameNumber       Play specific game\
\  ./wordle --how-to-play    Display instructions"

boundingLine = "###################"
boundOutside = "##"

defaultKeyboard = [
  ('q', "none"),
  ('w', "none"),
  ('e', "none"),
  ('r', "none"),
  ('t', "none"),
  ('y', "none"),
  ('u', "none"),
  ('i', "none"),
  ('o', "none"),
  ('p', "none"),
  ('a', "none"),
  ('s', "none"),
  ('d', "none"),
  ('f', "none"),
  ('g', "none"),
  ('h', "none"),
  ('j', "none"),
  ('k', "none"),
  ('l', "none"),
  ('z', "none"),
  ('x', "none"),
  ('c', "none"),
  ('v', "none"),
  ('b', "none"),
  ('n', "none"),
  ('m', "none")]

maxGuesses = 6
wordLength = 5

-- | Maintains the debug messages.
debugLog :: String -> IO ()
debugLog s =
  let debug = False in
  if debug then putStrLn s else return ()

-- | Returns true if there are too few letters.
notEnoughLetters :: String -> Bool
notEnoughLetters word =
  length word < wordLength

-- | Returns true if there are too many letters.
tooManyLetters :: String -> Bool
tooManyLetters word =
  length word > wordLength

-- | Returns true if every character in the word is a lowercase letter.
allLowercase :: String -> Bool
allLowercase [] = True
allLowercase (w:word) = 
  (toLower w == w) && (toUpper w /= w) && allLowercase word

-- | Returns true if the word is in the word list.
inWordList :: String -> [String] -> Bool
inWordList word wordList = 
  not (null (elemIndices word wordList))

-- | Returns true if the word satisfies all above conditions (some require false).
isValid :: String -> [String] -> Bool
isValid word wordList = 
    length word <= wordLength

-- | Returns a congradulatory caption based on the number of words the user guessed.
getCaption :: Int -> String
getCaption guessNum =
  case guessNum of
    1 -> "\nGenius!"
    2 -> "\nMagnificent!"
    3 -> "\nImpressive!"
    4 -> "\nSplendid!"
    5 -> "\nGreat!"
    6 -> "\nPhew!"
    _ -> "\n"

--------------------------------------------------------------------------------
-- Main

-- | Initializes the game of Wordle.
main :: IO ()
main = do {
  hSetBuffering stdin NoBuffering;
  args <- getArgs;
  wordList <- readFile "./words.txt";
  if length args > 1 then
    die tooManyArgs;
  else if length args == 1 then
    do {
      if head args == "--how-to-play" then
        putStrLn howToPlay;
     else if isJust(readMaybe (head args) :: Maybe Int)
      && (read (head args) >= 0)
      && (read (head args) < length (lines wordList)) then
        do {
            putStrLn "";
            view EmptyBoard;
            let answer = lines wordList !! read (head args);
            ;controller (WordleModel (lines wordList) answer [] "" [] 6 defaultKeyboard) "intro";
        }
      else 
        die "Invalid game number";
    }
  else 
    do {
      randIndex <- randomRIO (0, length (lines wordList) - 1);
      let answer = lines wordList !! randIndex;
      ;putStrLn "";
      view EmptyBoard;
      controller (WordleModel (lines wordList) answer [] "" [] 6 defaultKeyboard) "intro";
    }
}

{-- Figured out how to actually do let statements in do blocks (the answer
is curly braces for multiple variables). --}

--------------------------------------------------------------------------------
-- Controller

-- | Controls game flow.
controller :: Model -> String -> IO ()
controller model flag =
  let { wordList = modelWordList model;
        answer = modelAnswer model;
        board = modelBoard model;
        guesses = modelGuessesLeft model; 
        keyboard = modelKeyboard model} in
  do {
    if guesses <= 0 then 
      do {
        putStrLn ("Bummer, the answer was " ++ answer);
        die "No guesses remaining";
      }
    else 
      printPrompt flag;
      geese <- getChar;
      geese <- newChar geese; -- I cannot get this to work on Windows with enter properly
      if isAlpha geese then
          let { guess = map toLower (modelCurrGuess model);
          guessColor = map (charColor answer (guess ++ [geese])) 
          (zip [0..(length guess)] (guess ++ [geese])) } in
          do {
            putStrLn ">";
            view (update 
                              (KeyPress geese)
                              (WordleModel wordList answer board 
                              guess guessColor guesses keyboard));
            if answer == (guess ++ [geese]) then do {
              putStrLn (getCaption (modelGuessesLeft (update 
                (KeyPress geese)
                (WordleModel wordList answer 
                board guess guessColor guesses keyboard))));
              exitSuccess;
            }
            else do {
              putStrLn "";
              if length guess == 4 then
                printKeyboard (updateKeyboard (WordleModel wordList answer board 
                              guess guessColor guesses keyboard) (guess ++ [geese]));
              else return ();
              putStrLn "";
              putStrLn ("Current guess: " ++ (guess ++ [geese]));
              let { guessColor = map (charColor answer (guess ++ [geese])) 
                (zip [0..(length guess)] (guess ++ [geese]));
                guess = map toLower (modelCurrGuess model) } in
                if length guess == 4 then
                  controller (updateKeyboard (update 
                              (KeyPress geese) 
                              (WordleModel wordList answer board 
                              guess guessColor guesses keyboard)) 
                              guess) "";
                else controller (update (KeyPress geese) 
                              (WordleModel wordList answer board 
                              guess guessColor guesses keyboard)) "";    
          }
        }
      else
        do {
          putStrLn (">" ++ invalidWordHandler [geese] wordList);
          putStrLn "";
          view model;
          putStrLn "";
          controller (WordleModel wordList answer board "" 
          [] guesses keyboard) "";
        }
  }

-- Ignores newlines.
newChar :: Char -> IO Char
newChar '\n' = getChar
newChar c = return c

-- | Returns an appropriate message for an invalid word.
invalidWordHandler :: String -> [String] -> String
invalidWordHandler word wordList
  | tooManyLetters word = "Too many letters"
  | otherwise = "Non-alphabetic"

-- | Returns the next board model.
update :: Action -> Model -> Model
update _ EmptyBoard = EmptyBoard
update keyEv model = 
  let { wordList = modelWordList model;
  answer = modelAnswer model; 
  board = modelBoard model; 
  guess = modelCurrGuess model;
  guessColor = map (charColor answer guess)  (zip [0..(length guess - 1)] guess);
  guessesLeft = modelGuessesLeft model;
  keyboard = modelKeyboard model;
  acChar = cVal keyEv } in
    if length guess >= 5 then
      WordleModel wordList answer (board ++ [take 5 guess]) 
      "" guessColor (guessesLeft - 1) keyboard
    else if length guess >= 4 then
      WordleModel wordList answer (board ++ [guess ++ [acChar]]) 
      "" guessColor 
      (guessesLeft - 1) keyboard 
    else WordleModel wordList answer board (guess ++ [acChar]) guessColor
      guessesLeft keyboard

-- | Assigns a character the correct color.
charColor :: String -> String -> (Int, Char) -> (Char, String)
charColor answer guess (i, letter) =
  if answer!!i == letter then
    (letter, "green")
  else if null (elemIndices letter answer) then
    (letter, "gray")
  else if head (elemIndices letter answer) - 1 > length guess then
    (letter, "yellow")
  else
    if guess !! head (elemIndices letter answer) == letter then
      (letter, "gray")
    else
      (letter, "yellow")

-- | Add a letter to the keyboard.
addLetterKB :: (Char, String) -> [(Char, String)] -> [(Char, String)]
addLetterKB letter keyboard = 
  if snd letter == "none" then keyboard
  else if snd letter == "gray" then
    if null (elemIndices (fst letter, "none") keyboard) then keyboard
    else
      take (head (elemIndices (fst letter, "none") keyboard)) keyboard ++ [letter]
        ++ drop (head (elemIndices (fst letter, "none") keyboard) + 1) keyboard
  else if snd letter == "yellow" then
    if null (elemIndices (fst letter, "gray") keyboard) 
      && null (elemIndices (fst letter, "none") keyboard) then keyboard
    else if null (elemIndices (fst letter, "gray") keyboard) then
      take (head (elemIndices (fst letter, "none") keyboard)) keyboard ++ [letter]
            ++ drop (head (elemIndices (fst letter, "none") keyboard) + 1) keyboard
    else 
      take (head (elemIndices (fst letter, "gray") keyboard)) keyboard ++ [letter]
            ++ drop (head (elemIndices (fst letter, "gray") keyboard) + 1) keyboard
  else
    if null (elemIndices (fst letter, "gray") keyboard) 
      && null (elemIndices (fst letter, "yellow") keyboard)
      && null (elemIndices (fst letter, "none") keyboard) then keyboard
    else if not (null (elemIndices (fst letter, "gray") keyboard)) then
      take (head (elemIndices (fst letter, "gray") keyboard)) keyboard ++ [letter]
        ++ drop (head (elemIndices (fst letter, "gray") keyboard) + 1) keyboard
    else if not (null (elemIndices (fst letter, "none") keyboard)) then
      take (head (elemIndices (fst letter, "none") keyboard)) keyboard ++ [letter]
        ++ drop (head (elemIndices (fst letter, "none") keyboard) + 1) keyboard
    else
      take (head (elemIndices (fst letter, "yellow") keyboard)) keyboard ++ [letter]
        ++ drop (head (elemIndices (fst letter, "yellow") keyboard) + 1) keyboard

-- | Update a game's keyboard.
updateKeyboard :: Model -> String -> Model
updateKeyboard model origGuess =
  if null origGuess then model
  else if length origGuess == 1 then
    let { wordList = modelWordList model;
        answer = modelAnswer model;
        board = modelBoard model;
        guess = modelCurrGuess model;
        guessColor = modelGuessColor model; 
        guesses = modelGuessesLeft model;
        keyboard = modelKeyboard model } in 
      WordleModel wordList answer board guess guessColor guesses 
        (addLetterKB 
        (head origGuess, snd (guessColor !! (length guessColor - 1))) 
        keyboard)
  else 
    let { wordList = modelWordList model;
        answer = modelAnswer model;
        board = modelBoard model;
        (og:oguess) = origGuess;
        guess = modelCurrGuess model;
        (gc:guessColor) = modelGuessColor model; 
        guesses = modelGuessesLeft model;
        keyboard = modelKeyboard model } in
      updateKeyboard (WordleModel wordList answer board guess guessColor guesses 
          (addLetterKB 
          (og, snd gc)
          keyboard)) oguess
    
--------------------------------------------------------------------------------
-- View

-- | Prints the prompt to enter the next word (different on first execution).
printPrompt :: String -> IO ()
printPrompt "intro" =
  putStrLn "\nGuess the wordle!"
printPrompt str =
  putStrLn "Next guess?"

-- | Prints the current board state.
view :: Model -> IO ()
view EmptyBoard =
  do {
    putStrLn boundingLine;
    printBlankLines 6;
    putStrLn boundingLine; 
  }
view model =
  do { 
    putStrLn boundingLine;
    mapIO_ (printGuess (modelAnswer model)) (modelBoard model);
    printBlankLines (6 - length (modelBoard model));
    putStrLn boundingLine;
  }

-- | Prints a blank line for the board.
printBlankLine :: IO ()
printBlankLine = 
  do {
    putStr boundOutside;
    putStr "               ";
    putStrLn boundOutside
  }

-- | Prints x blank lines for the board.
printBlankLines :: Int -> IO ()
printBlankLines x
  | x < 0 =  die "Cannot print negative lines"
  | x == 0 = return ()
  | x == 1 = printBlankLine
  | otherwise = 
    do {
      printBlankLine;
      printBlankLines (x - 1)
    }

-- | Prints the guess, with letters formatted according to the answer.
printGuess :: String -> String -> IO ()
printGuess answer guess =
  do {
    putStr boundOutside;
    mapIO_ printLetter (map (charColor answer guess)
                            (zip [0..(length guess - 1)] guess));
    putStrLn boundOutside
  }

-- | Prints a letter of the guess compared to the answer.
printLetter :: (Char, String) -> IO ()
printLetter (letter, color) =
  if color == "green" then
    putStr ("\x1b[48;5;82m " ++ [letter] ++ " \x1b[0m")
  else if color == "gray" then
    putStr ("\x1b[48;5;243m " ++ [letter] ++ " \x1b[0m")
  else if color == "yellow" then
    putStr ("\x1b[48;5;226m " ++ [letter] ++ " \x1b[0m")
  else
    putStr (" " ++ [letter] ++ " ")

-- | Prints the current keyboard layout.
printKeyboard :: Model -> IO ()
printKeyboard model =
  let keyboard = modelKeyboard model in
    do {
      mapIO_ printLetter (take 10 keyboard);
      putStrLn "";
      mapIO_ printLetter (take 9 (drop 10 keyboard));
      putStrLn "";
      mapIO_ printLetter (drop 19 keyboard);
    }

-- | Maps an IO function onto a list and performs it.
mapIO_ :: (a -> IO b) -> [a] -> IO ()
mapIO_ f [] = return ()
mapIO_ f (a:as) = 
  do {
    f a;
    mapIO_ f as
  }