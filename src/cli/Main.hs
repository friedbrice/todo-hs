{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Text (pack)
import Data.UUID (UUID)

import Todo
import qualified Framework

main :: IO ()
main = Framework.makeCliApp (update updateCliApp) (view viewCli) initialModel
  where
  updateCliApp = Update{..}
  getTime = Framework.getTime
  newTaskId = TaskId . pack . show <$> Framework.random @UUID

  viewCli = View{..}
  heading = Framework.heading
  section = Framework.section
  text = Framework.text
  time = Framework.time
  textbox = Framework.textbox
  hfill = Framework.hfill
  vfill = Framework.vfill
  select = Framework.select
  switch = Framework.switch
  row = Framework.row
  col = Framework.col
  button = Framework.button
  modalWindow = Framework.modalWindow
  table = Framework.table
