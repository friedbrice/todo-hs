import Prelude hiding (putStrLn)

import Control.Monad.Reader
import Control.Monad.State
import Data.IORef

import Data.Text (pack)
import Data.Time.Clock (getCurrentTime)
import System.Random (randomIO)

import Todo

main :: IO ()
main = makeCliApp update view initialModel

makeCliApp :: (a -> c -> App c c) -> (c -> Cli c a) -> c -> IO ()
makeCliApp u v m =
  newIORef (Ctx m) >>= runApp mainLoop
  where
  mainLoop = do
    Ctx model <- get
    runCli (v model) $ \msg -> do
      model' <- u msg model
      put $ Ctx model'
      mainLoop

data Ctx c = Ctx c

newtype App c a = App { runApp :: IORef (Ctx c) -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO) via ReaderT (IORef (Ctx c)) IO

newtype Cli c a = Cli { runCli :: (a -> App c ()) -> App c () }
  deriving Functor

instance MonadState (Ctx c) (App c) where
  state f = App $ \ref -> do
    ctx <- readIORef ref
    let (a, ctx') = f ctx
    writeIORef ref ctx'
    return a

instance Update (App c) where
  getTime = liftIO getCurrentTime
  newTaskId = fmap (TaskId . pack . show @Int) (liftIO randomIO)

instance View (Cli c) where
  heading = undefined
  section = undefined
  text = undefined
  time = undefined
  textbox = undefined
  hfill = undefined
  vfill = undefined
  select = undefined
  switch = undefined
  row = undefined
  col = undefined
  button = undefined
  modalWindow = undefined
  table = undefined
