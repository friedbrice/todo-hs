import Todo

import Prelude hiding ((!!), getLine, putStr, putStrLn)

import Data.Foldable (fold, foldl')
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)
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

newtype Cli b = Cli { unCli :: [Either (IO ()) (IO b, Int -> IO ())] }
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
    n <- readMaybe @Int . unpack <$> getLine
    fromMaybe (promptRegistry registry) $ (registry !!) =<< n

static :: IO () -> Cli b
static draw = Cli [Left draw]

active :: IO b -> (Int -> IO ()) -> Cli b
active onPick draw = Cli [Right (onPick, draw)]

activate :: IO b -> Cli a -> Cli b
activate msg = Cli . fmap toActive1 . unCli
  where
  toActive1 (Left draw) = Right (msg, mkBtn (const draw))
  toActive1 (Right (_, draw)) = Right (msg, mkBtn draw)

  mkBtn draw n = do
    putStr "[ "
    draw n :: IO ()
    putStr " : "
    putStr . pack . show $ n
    putStr " ]"

instance View Cli where
  heading = text . ("\n# " <>) . (<> "\n")

  section = text . ("\n## " <>) . (<> "\n")

  text txt (Just b) = activate (pure b) (text txt Nothing)
  text txt Nothing = static (putStr txt)

  time utct (Just b) = activate (pure b) (time utct Nothing)
  time utct Nothing = static $ do
    tz <- getCurrentTimeZone
    putStr . pack . show $ utcToLocalTime tz utct

  textbox txt = active getLine $ \n ->
    putStr $ "[ "<> txt <> " :" <> (pack $ show n) <> " ]"

  hfill = static $ putStr "\t"

  vfill = static $ putStr "\n\n"

  select display (pre, sel, post) = undefined display pre sel post

  switch display (pre, sel, post) =
    row $ fmap mkSwitch pre <> [mkSwitch sel] <> fmap mkSwitch post
    where
    drawBtn draw n = do
      putStr "[ "
      draw n :: IO ()
      putStr " : "
      putStr . pack $ show n
      putStr " ]"

    redraw opt (Left draw) = Right (pure opt, drawBtn $ const draw)
    redraw opt (Right (_, draw)) = Right (pure opt, drawBtn draw)

    mkSwitch opt =
      let Cli cs = display opt in Cli $ redraw opt <$> cs

  row = fold . intercalate [static (putStr " | ")] . fmap pure

  col = fold . intercalate [static (putStr "\n")] . fmap pure

  button (Cli cs) = Cli $ buttonify <$> cs
    where
    buttonify (Left draw) =
      Left (do
        putStr "[ "
        draw :: IO ()
        putStr " ]")

    buttonify (Right (msg, draw)) =
      Right (msg, \n -> do
        putStr "[ "
        draw n :: IO ()
        putStr " : "
        putStr . pack $ show n
        putStr " ]")

  modalWindow = id

  table = undefined
