import Control.Monad
import Brick

import Todo

main :: IO ()
main = void $ defaultMain app initialModel

app :: App Model Message ()
app = App
  { appDraw = draw
  , appChooseCursor = chooseCursor
  , appHandleEvent = handleEvent
  , appStartEvent = startEvent
  , appAttrMap = attrMap'
  }

draw :: Model -> [Widget ()]
draw = undefined

chooseCursor :: Model -> [CursorLocation ()] -> Maybe (CursorLocation ())
chooseCursor = undefined

handleEvent :: Model -> BrickEvent () Message -> EventM () (Next Model)
handleEvent = undefined

startEvent :: Model -> EventM () Model
startEvent = undefined

attrMap' :: Model -> AttrMap
attrMap' = undefined
