{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Text (pack)
import Data.UUID (UUID)

import Todo
import qualified Cli.Framework  as Cli

main :: IO ()
main = Cli.makeCliApp (update updateCliApp) (view viewCli) initialModel
  where
  updateCliApp = Update{..}
  getTime = Cli.getTime
  newTaskId = TaskId . pack . show <$> Cli.random @UUID

  viewCli = View{..}
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
  button = Cli.button
  modalWindow = Cli.modalWindow
  table = Cli.table
