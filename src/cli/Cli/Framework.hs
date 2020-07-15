module Cli.Framework where

import Prelude hiding ((!!), getLine, putStr)

import Data.Foldable (fold, foldl')
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.Text (Text, pack, unpack)
import Data.Text.IO (getLine, putStr)
import Data.Time (UTCTime, getCurrentTimeZone, utcToLocalTime)
import System.Console.ANSI (clearScreen)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

makeCliApp :: (msg -> a -> IO a) -> (a -> Cli msg) -> a -> IO ()
makeCliApp update view model = do
  clearScreen
  let (registry, draw) = render $ view model
  draw *> hFlush stdout
  msg <- registryPrompt registry <* hFlush stdout
  newModel <- update msg model <* hFlush stdout
  makeCliApp update view newModel

data Element msg
  = Static (IO ())
  | Active (IO msg) (Int -> IO ())
  deriving Functor

newtype Cli msg = Cli [Element msg]
  deriving Functor
  deriving (Semigroup, Monoid) via [Element msg]

visit :: (Element a -> Element b) -> Cli a -> Cli b
visit f (Cli cs) = Cli $ fmap f cs

render :: Cli msg -> ([IO msg], IO ())
render (Cli cs) = (reverse revReg, drawAll)
  where
  (_, revReg, drawAll) = foldl' step (0, [], pure ()) cs
  step (n, regAcc, drawAcc) next = case next of
    Static draw -> (n, regAcc, drawAcc *> draw)
    Active msg draw -> (succ n, msg : regAcc, drawAcc *> draw n)

registryPrompt :: [IO msg] -> IO msg
registryPrompt registry =
  let
    [] !! _ = Nothing
    (x:xs) !! n
      | n == 0 = Just x
      | n > 0 = xs !! (n - 1)
      | otherwise = Nothing
  in do
    n <- readMaybe @Int . unpack <$> getLine
    fromMaybe (registryPrompt registry) $ (registry !!) =<< n

static :: IO () -> Cli msg
static draw = Cli [Static draw]

active :: IO msg -> (Int -> IO ()) -> Cli msg
active msg draw = Cli [Active msg draw]

activate :: IO msg -> Cli a -> Cli msg
activate msg = visit activate1
  where
  activate1 (Static draw) = Active msg . drawBtn $ const draw
  activate1 (Active _ draw) = Active msg $ drawBtn draw

  drawBtn draw n = do
    putStr "[ "
    draw n :: IO ()
    putStr " : "
    putStr . pack . show $ n
    putStr " ]"

hfill :: Cli msg
hfill = static $ putStr "\t"

vfill :: Cli msg
vfill = static $ putStr "\n\n"

row :: [Cli msg] -> Cli msg
row = fold . intercalate [static $ putStr " | "] . fmap pure

col :: [Cli msg] -> Cli msg
col = fold . (=<<) (\x -> [x, static $ putStr "\n"])

strong :: Cli a -> Cli a
strong = visit \case
  Static draw -> Static $ putStr "**" *> draw *> putStr "**"
  Active msg draw -> Active msg $ \n -> putStr "**" *> draw n *> putStr "**"

text :: Text -> Maybe msg -> Cli msg
text txt (Just msg) = activate (pure msg) $ text txt Nothing
text txt Nothing = static $ putStr txt

time :: UTCTime -> Maybe msg -> Cli msg
time utct (Just msg) = activate (pure msg) $ time utct Nothing
time utct Nothing = static do
  tz <- getCurrentTimeZone
  putStr . pack . show $ utcToLocalTime tz utct

textbox :: Text -> Cli Text
textbox txt = activate prompt $ text txt Nothing
  where prompt = putStr "Input: " *> hFlush stdout *> getLine

table :: [(Cli msg, a -> Cli msg)] -> [a] -> Cli msg
table cols rows = col $ headRow : dataRows
  where
  headRow = row $ fmap fst cols
  dataRows = fmap (row . dataRow) rows
  dataRow msg = fmap (($ msg) . snd) cols

select :: (a -> Cli void) -> ([a], a, [a]) -> Cli a
select draw (pre, sel, post) = activate msg $ draw sel
  where
  mkSwitch x = activate (pure x) $ draw x

  pre' = mkSwitch <$> pre
  sel' = mkSwitch sel
  post' = mkSwitch <$> post

  msg = do
    let (registry, drawAll) = render . col $ pre' <> [sel'] <> post'
    drawAll *> hFlush stdout
    registryPrompt registry <* hFlush stdout

switch :: (a -> Cli void) -> ([a], a, [a]) -> Cli a
switch draw (pre, sel, post) = row $ pre' <> [sel'] <> post'
  where
  mkSwitch f x = activate (pure x) . f $ draw x

  pre' = fmap (mkSwitch id) pre
  sel' = mkSwitch strong sel
  post' = fmap (mkSwitch id) post
