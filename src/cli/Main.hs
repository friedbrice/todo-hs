import Todo

import Prelude hiding (getLine, lookup, putStr)

import Data.Foldable (fold, foldl')
import Data.List (intercalate)
import Data.Map (Map, alter, lookup)
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
  b <- readRegistry registry
  newModel <- runCliApp $ u b m
  makeCliApp u v newModel

newtype CliApp a = CliApp { runCliApp :: IO a }
  deriving (Functor, Applicative, Monad) via IO

instance Update CliApp where
  getTime = CliApp $ getCurrentTime
  newTaskId = CliApp $ TaskId . pack . show <$> randomIO @UUID

newtype Cli b = Cli [(Int -> IO (), Maybe (IO b))]
  deriving Functor
  deriving (Semigroup, Monoid) via [(Int -> IO (), Maybe (IO b))]

render :: Cli b -> (Map Int (IO b), IO ())
render (Cli cs) = (registry, draw)
  where
  (_, registry, draw) = foldl' step (0 :: Int, mempty, mempty) cs

  step (n, regAcc, drawAcc) (drawNext, msgNext) =
    ( maybe n (const $ succ n) msgNext
    , alter (const msgNext) n regAcc
    , drawAcc *> drawNext n
    )

readRegistry :: Map Int (IO b) -> IO b
readRegistry registry = do
  n <- readMaybe . unpack <$> getLine
  fromMaybe (readRegistry registry) $ (`lookup` registry) =<< n

static :: IO () -> Cli b
static io = Cli [(const io, Nothing)]

active :: IO b -> (Int -> IO ()) -> Cli b
active b io = Cli [(io, Just b)]

btn :: Text -> Int -> Text
btn txt n = "[ "<> txt <> " | " <> (pack $ show n) <> " ]"

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

  select = undefined

  switch = undefined

  row = fold . intercalate [static (putStr " | ")] . fmap pure

  col = fold . intercalate [static (putStr "\n")] . fmap pure

  button = id

  modalWindow = id

  table = undefined
