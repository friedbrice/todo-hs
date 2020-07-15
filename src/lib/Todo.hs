{-# LANGUAGE RankNTypes #-}

module Todo where

import Control.Monad
import Data.List
import Data.Ord

import Data.Time (UTCTime)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as Text

import Todo.Display
import Todo.Enum


----
-- Model

newtype TaskId = TaskId Text
  deriving (Eq, Ord)

newtype Title = Title Text
  deriving Display via Text

data Priority
  = PriorityLow
  | PriorityNormal
  | PriorityHigh
  | PriorityUrgent
  deriving (Bounded, Enum, Eq, Ord, Show)
  deriving Display via DeriveDisplay Priority Priority

data Task = Task
  { description :: Description
  , created :: UTCTime
  , completed :: Maybe UTCTime
  }

data Description = Description
  { title :: Title
  , priority :: Priority
  }

data TaskFilter
  = TaskFilterAll
  | TaskFilterActive
  | TaskFilterCompleted
  deriving (Bounded, Enum, Eq, Show)
  deriving Display via DeriveDisplay TaskFilter TaskFilter

data TaskSort
  = SortPriority
  | SortCreated

data Model = Model
  { tasks :: Map TaskId Task
  , taskFilter :: TaskFilter
  , taskSort :: TaskSort
  , editor :: Maybe (Text, Maybe Priority, Maybe TaskId)
  }

initialModel :: Model
initialModel = Model
  { tasks = mempty
  , taskFilter = TaskFilterAll
  , taskSort = SortCreated
  , editor = Nothing
  }


----
-- Update

data Message
  = NewTask Description
  | UpdateTask TaskId Description
  | CompleteTask TaskId
  | DeleteTask TaskId
  | SetFilter TaskFilter
  | SetSort TaskSort
  | ClearCompleted
  | EditorOpen (Maybe TaskId)
  | EditorInputText Text
  | EditorInputPriority (Maybe Priority)
  | EditorClose

data Update u = Update
  { getTime :: u UTCTime
  , newTaskId :: u TaskId
  }

update :: Monad u => Update u -> Message -> Model -> u Model
update u@Update{..} msg model@Model{..} = case msg of

  NewTask description -> do
    taskId <- newTaskId
    created <- getTime
    update u EditorClose model{ tasks =
      Map.alter (const $ Just Task{ completed = Nothing, .. }) taskId tasks
    }

  UpdateTask taskId desc ->
    update u EditorClose model{ tasks =
      Map.alter (fmap \task -> task{ description = desc }) taskId tasks
    }

  CompleteTask taskId -> do
    cmpl <- Just <$> getTime
    return model{ tasks =
      Map.alter (fmap \task -> task { completed = cmpl }) taskId tasks
    }

  DeleteTask taskId ->
    return model{ tasks = Map.alter (const Nothing) taskId tasks }

  SetFilter f ->
    return model{ taskFilter = f }

  EditorOpen taskId ->
    return model{ editor =
      Just case (`Map.lookup` tasks) =<< taskId of
        Nothing ->
          ("", Nothing, Nothing)
        Just Task{ description = Description{ title = Title txt, .. } } ->
          (txt, Just priority, taskId)
    }

  EditorInputText input ->
    return model{ editor = fmap (\(_, x, y) -> (input, x, y)) editor }

  EditorInputPriority input ->
    return model{ editor = fmap (\(x, _, y) -> (x, input, y)) editor }

  EditorClose ->
    return model{ editor = Nothing }

  SetSort s ->
    return model{ taskSort = s }

  ClearCompleted ->
    return model{ tasks = Map.filter (\Task{..} -> null completed) tasks }


----
-- View

data View v = View
  { text :: forall msg. Text -> Maybe msg -> v msg
  , time :: forall msg. UTCTime -> Maybe msg -> v msg
  , textbox :: Text -> v Text
  , select :: forall a void. (a -> v void) -> ([a], a, [a]) -> v a
  , switch :: forall a void. (a -> v void) -> ([a], a, [a]) -> v a
  , table :: forall a msg. [(v msg, a -> v msg)] -> [a] -> v msg
  , hfill :: forall void. v void
  , vfill :: forall void. v void
  , row :: forall msg. [v msg] -> v msg
  , col :: forall msg. [v msg] -> v msg
  , strong :: forall msg. v msg -> v msg
  , button :: forall msg. v msg -> v msg
  , modal :: forall msg. v msg -> v msg
  }

view :: Functor v => View v -> Model -> v Message
view v@View{..} Model{..} =
  col $ [header, vfill, tasksTable, vfill, footer, vfill] <> editorModal
  where

  tasksTable = makeTable . sortTasks . filterTasks $ Map.toList tasks

  makeTable = table
    [ ( strong $ text "Title" Nothing
      , \(taskId, Task{ description = Description{..}, .. }) ->
        text (display title) case completed of
          Nothing -> Just . EditorOpen $ Just taskId
          Just _ -> Nothing
      )
    , ( strong $ text "Priority" (Just $ SetSort SortPriority)
      , \(taskId, Task{ description = Description{..}, .. }) ->
        text (display priority) case completed of
          Nothing -> Just . EditorOpen $ Just taskId
          Just _ -> Nothing
      )
    , ( strong $ text "Created" . Just $ SetSort SortCreated
      , \(_, Task{..}) -> time created Nothing
      )
    , ( strong  $ text "Completed" Nothing
      , \(taskId, Task{..}) -> case completed of
        Nothing -> text "-" . Just $ CompleteTask taskId
        Just x -> time x Nothing
      )
    ]

  sortTasks = case taskSort of
    SortCreated ->
      sortOn \(_, Task{ description = Description{..}, .. }) ->
        (created, Down priority)
    SortPriority ->
      sortOn \(_, Task{ description = Description{..}, .. }) ->
        (Down priority, created)

  filterTasks = case taskFilter of
    TaskFilterAll -> id
    TaskFilterActive -> filter \(_, Task{..}) -> null completed
    TaskFilterCompleted -> filter \(_, Task{..}) -> not $ null completed

  header = row
    [ hfill
    , strong $ text "TODO" Nothing
    , hfill
    , button . text "New task" . Just $ EditorOpen Nothing
    ]

  footer = row
    [ case length tasks of
      1 -> text "1 item left" Nothing
      n -> text (display n <> " items left") Nothing
    , hfill
    , fmap SetFilter
      . switch ((`text` Nothing) . display)
      $ fanEnum taskFilter
    , hfill
    , button . text "Clear completed" $ Just ClearCompleted
    ]

  editorModal = (`foldMap` editor) \(txt, pri, taskId) ->
    pure . modal $ col
      [ case taskId >>= (`Map.lookup` tasks) of
        Nothing -> strong $ text "New task" Nothing
        Just _ -> strong $ text "Edit task" Nothing
      , fmap EditorInputText $ textbox txt
      , fmap EditorInputPriority
        . select (viewMaybePriority v)
        $ fanEnum pri
      , row
        [ hfill
        , button . text "Cancel" $ Just EditorClose
        , button . text "Save" $ do
            desc <- validDescription txt pri
            case taskId of
              Just tid -> return $ UpdateTask tid desc
              Nothing -> return $ NewTask desc
        ]
      ]

validTitle :: Text -> Maybe Title
validTitle txt = guard (not $ Text.null txt) *> Just (Title txt)

validDescription :: Text -> Maybe Priority -> Maybe Description
validDescription t p = Description <$> validTitle t <*> p

viewMaybePriority :: View v -> Maybe Priority -> v a
viewMaybePriority View{..} mp = case mp of
  Nothing -> text "Select priority" Nothing
  Just p -> text (display p) Nothing
