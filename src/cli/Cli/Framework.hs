module Cli.Framework where

import Prelude hiding ((!!), getLine, putStr)

import Data.Foldable (fold, foldl')
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.Text (Text, pack, unpack)
import Data.Text.IO (getLine, putStr)
import Data.Time (UTCTime, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import System.Console.ANSI (clearScreen)
import System.IO (hFlush, stdout)
import System.Random (Random, randomIO)
import Text.Read (readMaybe)

makeCliApp :: (b -> a -> CliApp a) -> (a -> Cli b) -> a -> IO ()
makeCliApp update view model = do
  clearScreen
  let (registry, draw) = render (view model)
  draw *> hFlush stdout
  msg <- promptRegistry registry <* hFlush stdout
  newModel <- runCliApp (update msg model) <* hFlush stdout
  makeCliApp update view newModel

newtype CliApp a = CliApp { runCliApp :: IO a }
  deriving (Functor, Applicative, Monad) via IO

getTime :: CliApp UTCTime
getTime = CliApp $ getCurrentTime

random :: Random a => CliApp a
random = CliApp $ randomIO

type Element b = Either (IO ()) (IO b, Int -> IO ())

-- TODO: I should be distinguishing between inline and block elements
newtype Cli b = Cli [Element b]
  deriving Functor
  deriving (Semigroup, Monoid) via [Either (IO ()) (IO b, Int -> IO ())]

visit :: (Element a -> Element b) -> Cli a -> Cli b
visit f (Cli cs) = Cli (fmap f cs)

render :: Cli b -> ([IO b], IO ())
render = renderWith 0 []

renderWith :: Int -> [IO b] -> Cli b -> ([IO b], IO ())
renderWith n0 msgs (Cli cs) = (reverse revReg, drawAll)
  where
  (_, revReg, drawAll) = foldl' step (n0, reverse msgs, pure ()) cs
  step (n, regAcc, drawAcc) next = case next of
    Left draw -> (n, regAcc, drawAcc *> draw)
    Right (onPick, draw) -> (n + 1, onPick : regAcc, drawAcc *> draw n)

renderStatic :: Int -> Cli a -> IO ()
renderStatic n (Cli cs) =
  fold
  $ fmap (\x -> case x of
    Left draw -> draw
    Right (_, draw) -> draw n)
  $ cs

promptRegistry :: [IO b] -> IO b
promptRegistry registry =
  let
    [] !! _ = Nothing
    (x:xs) !! n
      | n == 0 = Just x
      | n > 0 = xs !! (n - 1)
      | otherwise = Nothing
  in do
    n <- readMaybe @Int . unpack <$> getLine
    fromMaybe (promptRegistry registry) $ (registry !!) =<< n

static :: IO () -> Cli b
static draw = Cli [Left draw]

active :: IO b -> (Int -> IO ()) -> Cli b
active onPick draw = Cli [Right (onPick, draw)]

activate :: IO b -> Cli a -> Cli b
activate msg = visit toActive1
  where
  toActive1 (Left draw) = Right (msg, mkBtn (const draw))
  toActive1 (Right (_, draw)) = Right (msg, mkBtn draw)

  mkBtn draw n = do
    putStr "[ "
    draw n :: IO ()
    putStr " : "
    putStr . pack . show $ n
    putStr " ]"

heading :: Text -> Maybe b -> Cli b
heading = text . ("\n# " <>) . (<> "\n")

section :: Text -> Maybe b -> Cli b
section = text . ("\n## " <>) . (<> "\n")

text :: Text -> Maybe b -> Cli b
text txt (Just b) = activate (pure b) (text txt Nothing)
text txt Nothing = static (putStr txt)

time :: UTCTime -> Maybe b -> Cli b
time utct (Just b) = activate (pure b) (time utct Nothing)
time utct Nothing = static $ do
  tz <- getCurrentTimeZone
  putStr . pack . show $ utcToLocalTime tz utct

textbox :: Text -> Cli Text
textbox txt = active getLine $ \n ->
  putStr $ "[ "<> txt <> " :" <> (pack $ show n) <> " ]"

hfill :: Cli b
hfill = static $ putStr "\t"

vfill :: Cli b
vfill = static $ putStr "\n\n"

select :: (a -> Cli a) -> ([a], a, [a]) -> Cli a
select display (pre, sel, post) = active msg (drawBtn drawSel)
  where
  drawBtn draw n = do
    putStr "[ "
    draw n :: IO ()
    putStr " : "
    putStr . pack $ show n
    putStr " ]"

  drawSel = (`renderStatic` display sel)

  redraw opt (Left draw) =
    Right (pure opt, drawBtn $ const draw)
  redraw opt (Right (_, draw)) =
    Right (pure opt, drawBtn draw)

  mkSwitch opt = visit (redraw opt) $ display opt

  pre' = mkSwitch <$> pre
  sel' = mkSwitch sel
  post' = mkSwitch <$> post

  msg = do
    let (registry, draw) = render . col $ pre' <> [sel'] <> post'
    draw *> hFlush stdout
    promptRegistry registry <* hFlush stdout

switch :: (a -> Cli a) -> ([a], a, [a]) -> Cli a
switch display (pre, sel, post) = row $ pre' <> [sel'] <> post'
  where
  drawBtn selected draw n = do
    putStr $ if selected then "[* " else "[ "
    draw n :: IO ()
    putStr " : "
    putStr . pack $ show n
    putStr $ if selected then " *]" else " ]"

  redraw selected opt (Left draw) =
    Right (pure opt, drawBtn selected $ const draw)
  redraw selected opt (Right (_, draw)) =
    Right (pure opt, drawBtn selected draw)

  mkSwitch selected opt = visit (redraw selected opt) $ display opt

  pre' = mkSwitch False <$> pre
  sel' = mkSwitch True sel
  post' = mkSwitch False <$> post

row :: [Cli b] -> Cli b
row = fold . intercalate [static (putStr " | ")] . fmap pure

col :: [Cli b] -> Cli b
col = fold . intercalate [static (putStr "\n")] . fmap pure

button :: Cli a -> Cli a
button = id

modalWindow :: Cli a -> Cli a
modalWindow = id

table :: [(Cli a, b -> Cli a)] -> [b] -> Cli a
table cols rows = col $ headRow : dataRows
  where
  headRow = row $ fmap fst cols
  dataRows = fmap (row . dataRow) rows
  dataRow b = fmap (($ b) . snd) cols

strong :: Cli a -> Cli a
strong (Cli cs) =
  Cli
  $ fmap (\x -> case x of
    Left draw ->
      Left $ putStr "**" *> draw *> putStr "**"
    Right (msg, draw) ->
      Right (msg, \n -> putStr "**" *> draw n *> putStr "**"))
  $ cs
