import Todo

import Prelude hiding ((!!), getLine, putStr)

import Data.Foldable (fold, foldl')
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text.IO (getLine, putStr)
import Data.Time (getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Data.UUID (UUID)
import System.Random (randomIO)
import Text.Read (readMaybe)

main :: IO ()
main = makeCliApp update view initialModel

makeCliApp :: (b -> a -> CliApp a) -> (a -> Cli b) -> a -> IO ()
makeCliApp u v m = do
  let (registry, draw) = render (v m)
  draw
  b <- promptRegistry registry
  newModel <- runCliApp $ u b m
  makeCliApp u v newModel

newtype CliApp a = CliApp { runCliApp :: IO a }
  deriving (Functor, Applicative, Monad) via IO

instance Update CliApp where
  getTime = CliApp $ getCurrentTime
  newTaskId = CliApp $ TaskId . pack . show <$> randomIO @UUID

newtype Cli b = Cli [Either (IO ()) (IO b, Int -> IO ())]
  deriving Functor
  deriving (Semigroup, Monoid) via [Either (IO ()) (IO b, Int -> IO ())]

render :: Cli b -> ([IO b], IO ())
render (Cli cs) = (reverse revReg, drawAll)
  where
  (_, revReg, drawAll) = foldl' step (0, [], pure ()) cs
  step (n, regAcc, drawAcc) next = case next of
    Left draw -> (n, regAcc, drawAcc *> draw)
    Right (onPick, draw) -> (n + 1, onPick : regAcc, drawAcc *> draw n)

promptRegistry :: [IO b] -> IO b
promptRegistry registry =
  let
    [] !! _ = Nothing
    (x:xs) !! n
      | n == 0 = Just x
      | n > 0 = xs !! (n - 1)
      | otherwise = Nothing
  in do
    n <- readMaybe . unpack <$> getLine
    fromMaybe (promptRegistry registry) $ (registry !!) =<< n

static :: IO () -> Cli b
static draw = Cli [Left draw]

active :: IO b -> (Int -> IO ()) -> Cli b
active onPick draw = Cli [Right (onPick, draw)]

btn :: Text -> Int -> Text
btn txt n = "[ "<> txt <> " :" <> (pack $ show n) <> " ]"

instance View Cli where
  heading = text . ("# " <>)

  section = text . ("## " <>)

  text txt Nothing = static $ putStr txt
  text txt (Just b) = active (pure b) $ putStr . btn txt

  time utct mMsg =
    let
      timeText = do
        tz <- getCurrentTimeZone
        return . pack . show $ utcToLocalTime tz utct
    in case mMsg of
      Nothing -> static $ putStr =<< timeText
      Just b -> active (pure b) $ \n -> do
        putStr =<< btn <$> timeText <*> pure n

  textbox txt = active getLine $ putStr . btn txt

  hfill = static $ putStr "\t"

  vfill = static $ putStr "\n\n"

  select display (pre, sel, post) = undefined display pre sel post

  switch display (pre, sel, post) = undefined display pre sel post

  row = fold . intercalate [static (putStr " | ")] . fmap pure

  col = fold . intercalate [static (putStr "\n")] . fmap pure

  button = id

  modalWindow = id

  table = undefined
