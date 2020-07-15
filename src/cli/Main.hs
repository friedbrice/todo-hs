import Data.Text (pack)
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import System.Random (randomIO)

import Todo
import qualified Cli.Framework  as Cli

main :: IO ()
main = Cli.makeCliApp (update Update{..}) (view View{..}) initialModel
  where
  getTime = getCurrentTime
  newTaskId = TaskId . pack . show <$> randomIO @UUID
  strong = Cli.strong
  text = Cli.text
  time = Cli.time
  textbox = Cli.textbox
  hfill = Cli.hfill
  vfill = Cli.vfill
  select = Cli.select
  switch = Cli.switch
  row = Cli.row
  col = Cli.col
  table = Cli.table
  button = id
  modal = id
