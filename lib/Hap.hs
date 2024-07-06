{-# Language BlockArguments #-}
{-# Language DeriveAnyClass #-}
{-# Language DerivingStrategies #-}
{-# Language DerivingVia #-}
{-# Language DuplicateRecordFields #-}
{-# Language ExplicitNamespaces #-}
{-# Language GADTs #-}
{-# Language KindSignatures #-}
{-# Language LambdaCase #-}
{-# Language MonadComprehensions #-}
{-# Language NoFieldSelectors #-}
{-# Language NumDecimals #-}
{-# Language OverloadedLabels #-}
{-# Language OverloadedRecordDot #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language QuantifiedConstraints #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}
{-# Language ViewPatterns #-}

{-# Options_ghc -Wno-unused-imports #-}

module Hap (module Hap) where

import Control.Concurrent (forkIO, forkOS)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.Chan qualified as Chan
import Control.Concurrent.STM.TBQueue (TBQueue, flushTBQueue)
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Control.Concurrent.STM.TChan (TChan, writeTChan)
import Control.Concurrent.STM.TChan qualified as TChan
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue, flushTQueue)
import Control.Concurrent.STM.TQueue qualified as TQueue
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVar, readTVar, stateTVar, writeTVar)
import Control.Concurrent.STM.TVar qualified as TVar
import Control.Exception (Exception, SomeException(SomeException), finally, throwIO, try)
import Control.Exception qualified as Exception
import Control.Monad ((<=<), join, void, when)
import Control.Monad.Except (MonadError, ExceptT)
import Control.Monad.Except qualified as Error
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Reader qualified as Reader
import Control.Monad.STM (STM, orElse)
import Control.Monad.STM qualified as STM
import Control.Monad.State.Strict (MonadState, StateT)
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Control.Monad.Writer (MonadWriter, WriterT)
import Control.Monad.Writer qualified as Writer
import Data.ByteString qualified as ByteString
import Data.Char qualified as Char
import Data.Coerce (coerce)
import Data.DList (DList)
import Data.DList qualified as DList
import Data.Either qualified as Lazy (partitionEithers)
import Data.Foldable (for_, sequenceA_, toList, traverse_)
import Data.Foldable qualified as Foldable (toList)
import Data.Function ((&), on)
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.Kind (Constraint, Type)
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Strict.Classes (toLazy, toStrict)
import Data.Strict.Either (Either(Left, Right))
import Data.Strict.Maybe (Maybe(Nothing, Just), maybe)
import Data.Strict.Tuple (type (:!:), pattern (:!:), fst, snd)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.IO qualified as Text.IO
import Data.Text.Read qualified as Text.Read
import Data.Traversable (for)
import Data.Void (Void)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Optics ((%~))
import Optics qualified
import Prelude hiding (Either(..), Maybe(..), Word, error, fst, id, lines, maybe, snd)
import Prelude qualified
import Prelude qualified as Lazy (Either(..), Maybe(..), fst, snd)
import Prettyprinter (Pretty(pretty))
import Prettyprinter qualified
import Prettyprinter.Render.Text qualified
import SDL (($=))
import SDL qualified
import System.Console.Haskeline qualified as Haskeline
import System.Console.Haskeline.IO qualified as Haskeline.IO
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPrint, hPutStrLn, stderr)
import System.Mem.Weak (Weak, deRefWeak, mkWeakPtr)
import System.Timeout qualified as Timeout

main :: IO ()
main = startIO defaultSettings

----------------------------------------------------------------

data Settings = Settings
  { input :: !Input,
    output :: !Output,
    sources :: !Sources
  }
  deriving stock (Generic, Show)

data Input
  = InputBatch
  | InputInteractive
  deriving stock (Generic, Show)

data Output
  = OutputGraphics
  | OutputText
  deriving stock (Generic, Show)

type Sources =
  Seq Source

data Source
  = SourcePath !FilePath
  | SourceText !Text
  deriving stock (Generic, Show)

defaultSettings :: Settings
defaultSettings =
  Settings
    { input = InputInteractive,
      output = OutputGraphics,
      sources = Seq.Empty
    }

startIO :: Settings -> IO ()
startIO settings = do
  messages <- messagesNew
  world <- worldNewIO
  compiledPrograms <- hapEvalIO
    world
    (compileAll settings.sources)

  let
    startInput = do
      hapEvalIO world do
        for_ compiledPrograms void
      exitCode <- case settings.input of
        InputBatch ->
          pure ExitSuccess
        InputInteractive ->
          consoleRunIO messages world
      messagesRequest messages (RequestExit exitCode)

    startOutput =
      case settings.output of
        OutputGraphics ->
          graphicsRun messages
        OutputText ->
          messagesLoop
            messages
            (pure ())
            (\_request -> pure ())

  exitWith =<< Async.withAsync startInput \_input ->
    startOutput

compileOne :: Source -> Code IO ProgramCompiled
compileOne source = do
  loaded <- (cellNew . programLoad) source
  tokenized <- (cellNew . fmap programTokenize . cellGet) loaded
  parsed <- (cellNew . (programParse <=< cellGet)) tokenized
  checked <- (cellNew . (programCheck <=< cellGet)) parsed
  compiled <- (fmap programCompile . cellGet) checked
  pure compiled

compileAll :: Sources -> Code IO (Seq ProgramCompiled)
compileAll sources = do
  loaded <- traverse (cellNew . programLoad) sources
  tokenized <- traverse (cellNew . fmap programTokenize . cellGet) loaded
  parsed <- traverse (cellNew . (programParse <=< cellGet)) tokenized
  checked <- traverse (cellNew . (programCheck <=< cellGet)) parsed
  compiled <- traverse (fmap programCompile . cellGet) checked
  pure compiled

----------------------------------------------------------------
--  Output Thread
----------------------------------------------------------------

data Messages =
  Messages {
    requests :: !(TBQueue Request)
  }
  deriving stock (Generic)

data Request
  = RequestOutput !Text
  | RequestGraphicsClear
  | RequestGraphicsBackgroundSet
    !(Word8) !(Word8) !(Word8) !(Word8)
  | RequestGraphicsPresent
  | RequestExit !ExitCode
  deriving stock (Generic, Show)

data Graphics =
  Graphics {
    window :: !SDL.Window,
    renderer :: !SDL.Renderer
  }
  deriving stock (Generic)

messagesNew :: IO Messages
messagesNew = do
  requests <- newTBQueueIO requestsCapacity
  pure Messages { requests }

requestsCapacity :: Natural
requestsCapacity = 1024

graphicsWith :: (Graphics -> IO a) -> IO a
graphicsWith = Exception.bracket graphicsBegin graphicsEnd

graphicsBegin :: IO Graphics
graphicsBegin = do
  SDL.initializeAll
  window <- SDL.createWindow "Hap" SDL.defaultWindow
  let firstSupportedDriver = -1
  renderer <- SDL.createRenderer
    window
    firstSupportedDriver
    SDL.defaultRenderer
  pure
    Graphics {
      window,
      renderer
    }

graphicsEnd :: Graphics -> IO ()
graphicsEnd graphics = do
  SDL.destroyWindow graphics.window

graphicsRun :: Messages -> IO ExitCode
graphicsRun messages =
  graphicsWith \graphics ->
    messagesLoop
      messages
      (graphicsPoll graphics messages)
      (graphicsRequest graphics)

graphicsPoll ::
  Graphics -> Messages -> ExceptT ExitCode IO ()
graphicsPoll graphics messages = do
  events <- liftIO SDL.pollEvents
  let
    (interruptions, payloads) =
      Lazy.partitionEithers (fmap eventParse events)
  case interruptions of
    exitCode : _ -> Error.throwError exitCode
    [] -> for_ payloads (graphicsEvent graphics)

graphicsEvent ::
  Graphics ->
  SDL.EventPayload ->
  ExceptT ExitCode IO ()
graphicsEvent graphics = \case
  SDL.WindowShownEvent{} -> do
    SDL.rendererDrawColor graphics.renderer $=
      SDL.V4 192 64 64 255
    SDL.clear graphics.renderer
    SDL.present graphics.renderer
  SDL.WindowExposedEvent{} -> do
    SDL.rendererDrawColor graphics.renderer $=
      SDL.V4 64 192 64 255
    SDL.clear graphics.renderer
    SDL.present graphics.renderer
  _ -> pure ()

graphicsRequest ::
  Graphics ->
  Request ->
  ExceptT ExitCode IO ()
graphicsRequest graphics = \case
  RequestOutput text -> lift do
    Text.IO.putStrLn text
  RequestGraphicsClear -> lift do
    SDL.clear graphics.renderer
  RequestGraphicsBackgroundSet r g b a -> lift do
    SDL.rendererDrawColor graphics.renderer $=
      SDL.V4 r g b a
    SDL.clear graphics.renderer
  RequestGraphicsPresent -> lift do
    SDL.present graphics.renderer
  RequestExit exitCode ->
    Error.throwError exitCode

messagesRequest ::
  (MonadIO m) => Messages -> Request -> m ()
messagesRequest messages request =
  atomically (TBQueue.writeTBQueue messages.requests request)

messagesLoop ::
  Messages ->
  ExceptT ExitCode IO () ->
  (Request -> ExceptT ExitCode IO ()) ->
  IO ExitCode
messagesLoop messages poll handle =
  fmap
    (either Prelude.id (const ExitSuccess))
    (Error.runExceptT loop)
  where
    loop = do
      poll
      requests <- lift do
        atomically do
          flushTBQueue messages.requests
      for_ requests handle
      loop

eventParse ::
  SDL.Event ->
  Lazy.Either ExitCode SDL.EventPayload
eventParse event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyboardEvent
    | let keyMotion = SDL.keyboardEventKeyMotion keyboardEvent,
      SDL.Pressed <- keyMotion,
      let keysym = SDL.keyboardEventKeysym keyboardEvent,
      SDL.KeycodeQ <- SDL.keysymKeycode keysym ->
      Lazy.Left ExitSuccess
  SDL.QuitEvent ->
    Lazy.Left ExitSuccess
  payload ->
    Lazy.Right payload

----------------------------------------------------------------
--  Reactive Cells
----------------------------------------------------------------

type Hap :: (Type -> Type) -> Constraint
class
  (
    Monad h,
    Monad (HapAtomic h)
  ) => Hap h where

  type family HapAtomic h :: Type -> Type
  data family HapVar h :: Type -> Type
  data family HapQueue h :: Type -> Type
  data family HapBoundedQueue h :: Type -> Type
  data family HapWeak h :: Type -> Type

  hapAtomic ::
    Atomic h a ->
    Code h a

  hapTry ::
    Code h a ->
    Code h (Either SomeException (a, Cells h))

  hapThrow ::
    (Exception e) =>
    e ->
    Code h bottom

  hapQueueWrite ::
    HapQueue h (Code h ()) ->
    Code h () ->
    Atomic h ()

  hapQueueFlush ::
    HapQueue h (Code h ()) ->
    Atomic h [Code h ()]

  hapVarNew ::
    a ->
    Atomic h (HapVar h a)

  hapVarAtomicNew ::
    a ->
    Code h (HapVar h a)
  hapVarAtomicNew = hapAtomic . hapVarNew

  hapVarRead ::
    HapVar h a ->
    Atomic h a

  hapVarState ::
    HapVar h a ->
    (a -> (b, a)) ->
    Atomic h b

  hapVarWrite ::
    HapVar h a ->
    a ->
    Atomic h ()

  hapVarModify ::
    HapVar h a ->
    (a -> a) ->
    Atomic h ()

  hapWeakNew ::
    a ->
    Code h (HapWeak h a)

  hapStrengthen ::
    HapWeak h a ->
    Code h (Maybe a)

type Atomic :: (Type -> Type) -> Type -> Type
type Atomic h = CodeT h (HapAtomic h)

type Code :: (Type -> Type) -> Type -> Type
type Code h = CodeT h h

instance Hap IO where

  type instance HapAtomic IO = STM

  newtype instance HapVar IO a =
    VarIO { var :: TVar a }

  newtype instance HapQueue IO a =
    QueueIO { queue :: TQueue a }

  newtype instance HapBoundedQueue IO a =
    BoundedQueueIO { bqueue :: TBQueue a }

  newtype instance HapWeak IO a =
    WeakIO { weak :: Weak a }

  hapAtomic code =
    CodeT \world -> atomically (code.execute world)

  hapTry code =
    CodeT \world -> do
      results <- try (code.execute world)
      pure (toStrict results, DList [])

  hapThrow = throw

  hapQueueWrite = fmap lift . writeTQueue . (.queue)

  hapQueueFlush = lift . flushTQueue . (.queue)

  hapVarRead = lift . readTVar . (.var)

  hapVarWrite = fmap lift . writeTVar . (.var)

  hapVarState = fmap lift . stateTVar . (.var)

  hapVarModify = fmap lift . modifyTVar' . (.var)

  hapVarNew = lift . fmap VarIO . newTVar

  hapVarAtomicNew = liftIO . fmap VarIO . newTVarIO

  hapWeakNew = liftIO . fmap WeakIO . (`mkWeakPtr` Lazy.Nothing)

  hapStrengthen = liftIO . fmap toStrict . deRefWeak . (.weak)

type CodeT :: (Type -> Type) -> (Type -> Type) -> Type -> Type
newtype CodeT h m a where
  CodeT ::
    {
      execute :: World h -> m (a, Cells h)
    } -> CodeT h m a
  deriving
    (
      Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadReader (World h),
      MonadWriter (Cells h)
    ) via (ReaderT (World h) (WriterT (Cells h) m))

instance MonadTrans (CodeT h) where
  lift action = CodeT \_world -> do
    result <- action
    pure (result, DList [])

type Cells h = DList (CellSome h)

type World :: (Type -> Type) -> Type
data World h =
  World {
    allocator :: !(HapVar h Id),
    heap :: !(HapVar h (IdMap (CellSome h))),
    queue :: !(HapQueue h (Code h ()))
  }
  deriving stock (Generic)

type CellSome :: (Type -> Type) -> Type
data CellSome h where
  CellSome ::
    {
      cell :: !(Cell h a)
    } -> CellSome h

instance Show (CellSome h) where
  showsPrec p CellSome{} =
    showParen (p >= 10) (showString "CellSome _")

type CellWeak :: (Type -> Type) -> Type
data CellWeak h where
  CellWeak ::
    {
      weak :: !(HapWeak h (Cell h a))
    } -> CellWeak h

type Cell :: (Type -> Type) -> Type -> Type
data Cell h a =
  Cell {
    code :: !(HapVar h (Code h a)),
    getters :: !(HapVar h (Seq (CellWeak h))),
    cache :: !(HapVar h (Cache a))
  }
  deriving stock (Generic)

deriving stock instance
  (
    Hap h,
    forall x. Show (HapVar h x),
    Show a
  ) => Show (Cell h a)

data Cache a
  --  IDEA: Split 'CacheFull' to save an indirection.
  = CacheFull !(Result a)
  | CacheEmpty
  | CacheFilling

type Result =
  Either SomeException

interpret :: Text -> IO (Maybe Value)
interpret text = do
  world <- worldNewIO
  let
    timeout_s = 2
    micro n = 1e6 * n
  fmap toStrict do
    Timeout.timeout (micro timeout_s) do
      hapEvalIO world do
        compiled <- compileOne (SourceText text)
        compiled

hapEvalIO :: World IO -> Code IO a -> IO a
hapEvalIO world code = fmap Lazy.fst (code.execute world)

hapRunIO :: World IO -> Code IO a -> IO (a, Cells IO)
hapRunIO world code = code.execute world

worldNewIO :: IO (World IO)
worldNewIO = do
  allocator <- idSourceNewIO
  heap <- heapNewIO
  queue <- queueNewIO
  pure World { allocator, heap, queue }

heapNewIO :: IO (HapVar IO (IdMap (CellSome IO)))
heapNewIO = fmap VarIO (newTVarIO idMapEmpty)

queueNewIO :: IO (HapQueue IO (Code IO ()))
queueNewIO = fmap QueueIO newTQueueIO

enqueue ::
  (Hap h) =>
  Code h () ->
  Code h ()
enqueue action = do
  queue <- Reader.asks (.queue)
  hapAtomic do
    hapQueueWrite queue action

flush ::
  (Hap h) =>
  Code h ()
flush = do
  queue <- Reader.asks (.queue)
  actions <- hapAtomic do
    hapQueueFlush queue
  sequenceA_ actions

notifyPut ::
  (Hap h) =>
  Cell h a ->
  Code h ()
notifyPut =
  traverse_
    (traverse_ (\CellSome { cell } -> notifyPut cell) <=<
      cellStrengthen) <=<
  (hapAtomic . hapVarRead) .
  (.getters)

cellStrengthen ::
  (Hap h) =>
  CellWeak h ->
  Code h (Maybe (CellSome h))
cellStrengthen CellWeak { weak } =
  fmap (fmap CellSome) (hapStrengthen weak)

cellGet ::
  (Hap h) =>
  Cell h a ->
  Code h a
cellGet cell = do
  continue <- hapAtomic do
    cache <- hapVarRead cell.cache
    case cache of
      CacheFull (Right value) -> pure do
        Writer.tell (DList [CellSome cell])
        pure value
      CacheFull (Left error) -> pure do
        hapThrow error
      CacheEmpty -> do
        hapVarWrite cell.cache CacheFilling
        code <- hapVarRead cell.code
        pure do
          result <- hapTry code
          case result of
            Left error -> do
              hapAtomic do
                hapVarWrite cell.cache (CacheFull (Left error))
              hapThrow error
            Right (value, putters) -> do
              getter <- cellWeakNew cell
              hapAtomic do
                hapVarWrite cell.cache (CacheFull (Right value))
                for_ putters \(CellSome putter) ->
                  hapVarModify putter.getters (getter :<|)
              Writer.tell (DList [CellSome cell])
              pure value
      CacheFilling -> pure do
        hapThrow Cycle
  continue

cellPut ::
  (Hap h) =>
  Cell h a ->
  Code h a ->
  Code h ()
cellPut cell code = do
  hapAtomic do
    hapVarWrite cell.code code
  notifyPut cell

data Cycle
  = Cycle
  deriving anyclass (Exception)
  deriving stock (Show)

cellOf ::
  (Hap h) =>
  a ->
  Code h (Cell h a)
cellOf = cellNew . pure

cellNew ::
  (Hap h) =>
  Code h a ->
  Code h (Cell h a)
cellNew = fmap (.withoutId) . cellNewWithId

cellNewWithId ::
  (Hap h) =>
  Code h a ->
  Code h (WithId (Cell h a))
cellNewWithId code0 = do
  allocator <- Reader.asks (.allocator)
  id <- idNew allocator
  code <- hapVarAtomicNew code0
  getters <- hapVarAtomicNew Seq.Empty
  cache <- hapVarAtomicNew CacheEmpty
  let
    cell =
      Cell {
        code,
        getters,
        cache
      }
  do
    heap <- Reader.asks (.heap)
    hapAtomic do
      hapVarModify heap
        (idMapInsert id (CellSome cell))
  pure WithId { id, withoutId = cell }

cellWeakNew :: (Hap h) => Cell h a -> Code h (CellWeak h)
cellWeakNew cell = fmap CellWeak (hapWeakNew cell)

----------------------------------------------------------------
--  Cell IDs
----------------------------------------------------------------

newtype Id = Id { number :: Nat }
  deriving newtype (Enum)
  deriving stock (Generic, Show)

data WithId a =
  WithId {
    id :: Id,
    withoutId :: a
  }
  deriving stock (Generic, Show)

newtype IdMap a =
  IdMap {
    map :: IntMap a
  }
  deriving stock (Generic, Show)

idMapEmpty :: IdMap a
idMapEmpty = IdMap IntMap.empty

idMapInsert :: Id -> a -> IdMap a -> IdMap a
idMapInsert id value ids =
  ids & #map %~
    IntMap.insert (fromIntegral id.number) value

newtype IdSet =
  IdSet {
    set :: IntSet
  }
  deriving stock (Generic, Show)

idSourceNewIO :: IO (HapVar IO Id)
idSourceNewIO = fmap VarIO (newTVarIO (Id 0))

idNew :: (Hap h) => HapVar h Id -> Code h Id
idNew source = hapAtomic do
  hapVarState source next
  where
    next id0 = (id0, id1)
      where
        !id1 = succ id0

----------------------------------------------------------------
--  Console
----------------------------------------------------------------

data Console =
  Console {
    messages :: !Messages,
    inputState :: !Haskeline.IO.InputState,
    world :: !(World IO)
  }

consoleRunIO :: Messages -> World IO -> IO ExitCode
consoleRunIO messages world = do
  inputState <- Haskeline.IO.initializeInput
    Haskeline.defaultSettings
  let console = Console { messages, inputState, world }
  Reader.runReaderT consoleLoop console
    `finally` Haskeline.IO.cancelInput inputState

type Consoled = ReaderT Console IO

consoleLoop :: Consoled ExitCode
consoleLoop = do
  let prompt = "> "
  lines <- consoleInput prompt
  case lines of
    Lazy.Nothing ->
      pure ExitSuccess
    Lazy.Just line -> case parseComment line of
      Lazy.Just comment -> case parseCommand comment of
        Just CommandQuit ->
          pure ExitSuccess
        Nothing -> do
          consoleOutput "unknown command"
          consoleLoop
      Lazy.Nothing -> do
        world <- Reader.asks (.world)
        result <- liftIO do
          hapEvalIO world do
            join (compileOne (SourceText line))
        consoleOutput (Text (show result))
        consoleLoop

consoleInput :: String -> Consoled (Lazy.Maybe Text)
consoleInput prompt = do
  inputState <- Reader.asks (.inputState)
  fmap (fmap Text) do
    lift do
      Haskeline.IO.queryInput inputState do
        Haskeline.getInputLine prompt

consoleOutput :: Text -> Consoled ()
consoleOutput (Text string) = do
  inputState <- Reader.asks (.inputState)
  lift do
    Haskeline.IO.queryInput inputState do
      Haskeline.outputStrLn string

parseComment :: Text -> Lazy.Maybe Text
parseComment =
  Text.stripPrefix commentPrefix . Text.stripStart

commentPrefix :: Text
commentPrefix = "//"

data Command
  = CommandQuit
  deriving stock (Generic, Show)

parseCommand :: Text -> Maybe Command
parseCommand input0 = case Text.strip input0 of
  "quit" -> Just CommandQuit
  _ -> Nothing

----------------------------------------------------------------
--  Prelude Renaming
----------------------------------------------------------------

type Nat = Prelude.Word

----------------------------------------------------------------
--  Character Set
----------------------------------------------------------------

pattern CharLineFeed :: Char
pattern CharLineFeed = '\x000A'

pattern CharQuotationMark :: Char
pattern CharQuotationMark = '\x0022'

pattern CharLeftParenthesis :: Char
pattern CharLeftParenthesis = '\x0028'

pattern CharRightParenthesis :: Char
pattern CharRightParenthesis = '\x0029'

pattern CharFullStop :: Char
pattern CharFullStop = '\x002E'

pattern CharLeftCurlyBracket :: Char
pattern CharLeftCurlyBracket = '\x007B'

pattern CharRightCurlyBracket :: Char
pattern CharRightCurlyBracket = '\x007D'

charIsTextEnd :: Char -> Bool
charIsTextEnd = \case
  CharLineFeed -> True
  CharQuotationMark -> True
  _ -> False

charIsTokenBoundary :: Char -> Bool
charIsTokenBoundary = \case
  CharQuotationMark -> True
  CharLeftParenthesis -> True
  CharRightParenthesis -> True
  char
    | Char.isSpace char -> True
  _ -> False

----------------------------------------------------------------
--  Lexical Syntax
----------------------------------------------------------------

pattern KeywordVar :: Text
pattern KeywordVar = "var"

----------------------------------------------------------------
--  Convenience Patterns
----------------------------------------------------------------

{-# Complete DList #-}

pattern DList :: [a] -> DList a
pattern DList list <- (DList.toList -> list)
  where
    DList list = DList.fromList list

{-# Complete Map #-}

pattern Map :: (Ord k, Show k) => [(k, v)] -> Map k v
pattern Map assocs <- (Map.toList -> assocs)
  where
    Map assocs =
      Map.fromListWithKey
        (\key _new _old ->
          Prelude.error ("duplicate key: " <> show key))
        assocs

{-# Complete Seq #-}

pattern Seq :: [a] -> Seq a
pattern Seq list <- (Foldable.toList -> list)
  where
    Seq list = Seq.fromList list

{-# Complete Text #-}

pattern Text :: String -> Text
pattern Text string <- (Text.unpack -> string)
  where
    Text string = Text.pack string

{-# Complete TextCons, TextEmpty #-}

pattern TextCons :: Char -> Text -> Text
pattern TextCons char text <-
  (Text.uncons -> Lazy.Just (char, text))
  where
    TextCons char text = Text.cons char text

pattern TextEmpty :: Text
pattern TextEmpty <- (Text.null -> True)
  where
    TextEmpty = Text.empty

----------------------------------------------------------------
--  Containers
----------------------------------------------------------------

newtype Union a = Union { union :: a }

instance
  (
    Ord k,
    Semigroup v
  ) =>
  Monoid (Union (Map k v)) where
  mempty = Union Map.empty

instance
  (
    Ord k,
    Semigroup v
  ) =>
  Semigroup (Union (Map k v)) where
  a <> b = Union (Map.unionWith (<>) a.union b.union)

adjacent :: [a] -> [(a, a)]
adjacent = zip <*> drop 1

adjacentMaybe :: [a] -> [(a, Maybe a)]
adjacentMaybe = \case
  x0 : xs0@(x1 : _xs1) -> (x0, Just x1) : adjacentMaybe xs0
  [x0] -> [(x0, Nothing)]
  [] -> []

----------------------------------------------------------------
--  MonadIO Wrappers
----------------------------------------------------------------

atomically :: (MonadIO m) => STM a -> m a
atomically = liftIO . STM.atomically

newTBQueueIO :: (MonadIO m) => Natural -> m (TBQueue a)
newTBQueueIO = liftIO . TBQueue.newTBQueueIO

newTChanIO :: (MonadIO m) => m (TChan a)
newTChanIO = liftIO TChan.newTChanIO

newTQueueIO :: (MonadIO m) => m (TQueue a)
newTQueueIO = liftIO TQueue.newTQueueIO

newTVarIO :: (MonadIO m) => a -> m (TVar a)
newTVarIO = liftIO . TVar.newTVarIO

readTVarIO :: (MonadIO m) => TVar a -> m a
readTVarIO = liftIO . TVar.readTVarIO

throw :: (Exception e, MonadIO m) => e -> m z
throw = liftIO . throwIO

----------------------------------------------------------------
--  Pretty Printing
----------------------------------------------------------------

type Doc = Prettyprinter.Doc Void

prettyText :: Doc -> Text
prettyText =
  Prettyprinter.Render.Text.renderStrict .
  Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions

----------------------------------------------------------------
--  Programs
----------------------------------------------------------------

type ProgramLoaded = Text
type ProgramTokenized = Tokens
type ProgramParsed = Parsed Block
type ProgramChecked = Checked IO Block
type ProgramCompiled = Code IO Value

type Tokens = [Token]

data Token
  = TokenWord !Text
  | TokenKeyword !Text
  | TokenText !Text
  | TokenGroupBegin
  | TokenGroupEnd
  | TokenBlockBegin
  | TokenBlockEnd
  deriving stock (Generic, Show)

newtype Expression annotation where
  Expression :: { terms :: Terms a } -> Expression a
  deriving stock (Generic, Show)

type Terms a = Seq (Term a)

data Term annotation where
  TermName :: !a -> !Name -> Term a
  TermVar :: !a -> !Name -> Term a
  TermValue :: !Value -> Term a
  TermGroup :: !a -> !(Expression a) -> Term a
  TermBlock :: !a -> !(Block a) -> Term a
  deriving stock (Generic, Show)

type Obj :: (Type -> Type) -> Type
data Obj h
  = ObjObj
  | ObjArr !(Arr h)
  deriving stock (Generic)

deriving stock instance
  (
    Hap h,
    forall x. Show (HapVar h x)
  ) => Show (Obj h)

type Objs :: (Type -> Type) -> Type
data Objs h
  = ObjsNil
  | ObjsCons
    !(Cell h (Maybe (Obj h)))
    !(Cell h (Maybe (Objs h)))
  deriving stock (Generic)

deriving stock instance
  (
    Hap h,
    forall x. Show (HapVar h x)
  ) => Show (Objs h)

type Arr :: (Type -> Type) -> Type
data Arr h
  = Arr {
    input, output :: !(Cell h (Maybe (Objs h)))
  }
  deriving stock (Generic)

deriving stock instance
  (
    Hap h,
    forall x. Show (HapVar h x)
  ) => Show (Arr h)

data Value
  = ValueNumber !Number
  | ValueText !Text
  deriving stock (Generic, Show)

data Uninitialized
  = Uninitialized
  deriving anyclass (Exception)
  deriving stock (Generic, Show)

data Block annotation where
  Block ::
    {
      scope :: !(Scope a),
      body :: !(Expression a)
    } -> Block a
  deriving stock (Generic, Show)

type Scope = Map Name

newtype Name =
  Name { spelling :: Text }
  deriving stock (Eq, Generic, Ord, Show)

type Number = Integer

data ParseError
  = ParseError
  deriving anyclass (Exception)
  deriving stock (Generic, Show)

programLoad :: Source -> Code IO ProgramLoaded
programLoad = \case
  SourcePath path ->
    fmap Text.Encoding.decodeUtf8 do
      liftIO (ByteString.readFile path)
  SourceText text ->
    pure text

----------------------------------------------------------------
--  Tokenization
----------------------------------------------------------------

programTokenize :: ProgramLoaded -> ProgramTokenized
programTokenize = \case
  TextCons char chars1
    | CharQuotationMark <- char -> let
      (text, chars2) = Text.break charIsTextEnd chars1
      in TokenText text : programTokenize (Text.drop 1 chars2)
    | Just token <- matchPunctuation char ->
      token : programTokenize chars1
    where
      matchPunctuation = \case
        CharLeftParenthesis -> Just TokenGroupBegin
        CharRightParenthesis -> Just TokenGroupEnd
        CharLeftCurlyBracket -> Just TokenBlockBegin
        CharRightCurlyBracket -> Just TokenBlockEnd
        _ -> Nothing
  chars0
    | TextEmpty <- word,
      TextEmpty <- chars1 ->
      []
    | TextEmpty <- word ->
      programTokenize (Text.stripStart chars1)
    | otherwise ->
      token : programTokenize chars1
    where
      (word, chars1) = Text.break charIsTokenBoundary chars0
      token
        | TextCons CharFullStop keyword <- word =
          TokenKeyword keyword
        | otherwise =
          TokenWord word

----------------------------------------------------------------
--  Parsing
----------------------------------------------------------------

type Parsed f = f ()

type ParseResult a = (a, BindingSites, Tokens)

type BindingSites = Union (Map Name (Seq ()))

programParse :: ProgramTokenized -> Code IO ProgramParsed
programParse = parseAllTerms

parseAllTerms :: (MonadIO m) => Tokens -> m ProgramParsed
parseAllTerms tokens0
  | null tokens1 =
    pure (Block scope (Expression (Seq terms)))
  | otherwise =
    throw ParseError
  where
    (terms, sites, tokens1) = parseManyTerms tokens0
    scope = scopeFromSites sites

--  IDEA: Handle duplicate bindings and annotations here.
scopeFromSites :: BindingSites -> Parsed Scope
scopeFromSites = void . (.union)

parseManyTerms :: Tokens -> ParseResult [Parsed Term]
parseManyTerms tokens0 =
  case Error.runExcept (parseOneTerm tokens0) of
    Lazy.Left{} ->
      ([], mempty, tokens0)
    Lazy.Right (term, sites0, tokens1) ->
      (term : terms, sites2, tokens2)
      where
        ~(terms, sites1, tokens2) = parseManyTerms tokens1
        sites2 = sites0 <> sites1

parseOneTerm ::
  (MonadError ParseError m) =>
  Tokens ->
  m (ParseResult (Parsed Term))
parseOneTerm = \case
  [] -> Error.throwError ParseError
  token : tokens1 -> case token of
    TokenKeyword KeywordVar
      | TokenWord word : tokens2 <- tokens1 -> let
        name = Name word
        in pure
          (
            TermVar () name,
            (Union . Map) [(name, Seq [()])],
            tokens2
          )
      | otherwise -> Error.throwError ParseError
    TokenKeyword keyword ->
      Error.throwError ParseError
    TokenWord word ->
      pure (term, mempty, tokens1)
      where
        term = maybe
          (TermName () (Name word))
          (TermValue . ValueNumber)
          (parseNumber word)
    TokenText text ->
      pure (TermValue (ValueText text), mempty, tokens1)
    TokenGroupBegin -> case tokens2 of
      TokenGroupEnd : tokens3 ->
        pure
          (
            TermGroup () (Expression (Seq terms)),
            sites,
            tokens3
          )
      _ ->
        Error.throwError ParseError
      where
        (terms, sites, tokens2) = parseManyTerms tokens1
    TokenGroupEnd ->
      Error.throwError ParseError
    TokenBlockBegin -> case tokens2 of
      TokenBlockEnd : tokens3 ->
        pure (block, mempty, tokens3)
        where
          block = TermBlock ()
            (Block scope (Expression (Seq terms)))
      _ ->
        Error.throwError ParseError
      where
        (terms, sites, tokens2) = parseManyTerms tokens1
        scope = scopeFromSites sites
    TokenBlockEnd ->
      Error.throwError ParseError

parseNumber :: Text -> Maybe Number
parseNumber word
  | Lazy.Right (number, TextEmpty) <-
      Text.Read.signed Text.Read.decimal word =
    Just number
  | otherwise =
    Nothing

----------------------------------------------------------------
--  Checking
----------------------------------------------------------------

type Checked ::
  (Type -> Type) ->
  (Type -> Type) ->
  Type
type Checked h f = f (Arr h)

data CheckError
  = CheckError
  deriving anyclass (Exception)
  deriving stock (Generic, Show)

type WithArr h = (:!:) (Arr h)

programCheck :: ProgramParsed -> Code IO ProgramChecked
programCheck =
  fmap snd . checkBlock (mempty :: Checked IO Scope)

checkBlock ::
  Checked IO Scope ->
  Parsed Block ->
  Code IO (WithArr IO (Checked IO Block))
checkBlock scope0 block = do
  scope <- fmap Map do
    for (Map.toList block.scope) checkBinding
  let
    -- IDEA: Allow shadowing to be a warning.
    scope1 = Map.unionWithKey
      (\key _old _new ->
        Prelude.error ("shadowed variable: " <> show key))
      scope0
      scope
  arr :!: body <- checkExpression scope1 block.body
  flush
  pure (arr :!: Block { scope, body })

checkBinding :: (Name, ()) -> Code IO (Name, Arr IO)
checkBinding (name, ()) = do
  arr <- Arr
    <$> cellOf Nothing
    <*> cellOf Nothing
  pure (name, arr)

--  IDEA: Memoize to avoid allocating a lot of new cells.
checkTerm ::
  Checked IO Scope ->
  Parsed Term ->
  Code IO (WithArr IO (Checked IO Term))
checkTerm scope0 = \case
  TermName () name -> do
    arr <- checkName scope0 name
    flush
    pure (arr :!: TermName arr name)
  TermVar () name -> do
    arr <- checkName scope0 name
    flush
    pure (arr :!: TermVar arr name)
  TermValue value -> do
    arr <- do
      obj <- cellOf (Just ObjObj)
      input <- cellOf Nothing
      output <- cellOf (Just (ObjsCons obj input))
      pure Arr { input, output }
    pure (arr :!: TermValue value)
  TermGroup () body0 -> do
    arr :!: body1 <- checkExpression scope0 body0
    flush
    pure (arr :!: TermGroup arr body1)
  TermBlock () block0 -> do
    arr0 :!: block1 <- checkBlock scope0 block0
    flush
    arr1 <- do
      obj <- cellOf (Just (ObjArr arr0))
      input <- cellOf Nothing
      output <- cellOf (Just (ObjsCons obj input))
      pure Arr { input, output }
    pure (arr1 :!: TermBlock arr1 block1)

checkExpression ::
  Checked IO Scope ->
  Parsed Expression ->
  Code IO (WithArr IO (Checked IO Expression))
checkExpression scope0 expression = do
  terms1 <- for expression.terms (checkTerm scope0)
  flush
  identityArr <- do
    objs <- cellOf Nothing
    pure (Arr objs objs)
  arr <- (`State.execStateT` identityArr) do
    for_ terms1 \(producerArr :!: _producer) -> do
      consumerArr <- State.get
      composedArr <- lift do
        checkCompose consumerArr producerArr
      State.put composedArr
  flush
  pure (arr :!: Expression (fmap snd terms1))

checkCompose :: Arr IO -> Arr IO -> Code IO (Arr IO)
checkCompose consumer producer = do
  input0 <- cellGet consumer.input
  output0 <- cellGet producer.output
  for_ ((liftA2 (,) `on` toLazy) input0 output0)
    (uncurry checkObjs)
  flush
  pure
    Arr {
      input = producer.input,
      output = consumer.output
    }

checkObjs :: Objs IO -> Objs IO -> Code IO ()
checkObjs ObjsNil ObjsNil = pure ()
checkObjs (ObjsCons x xs) (ObjsCons y ys) = do
  checkMaybe checkObj x y
  flush
  checkMaybe checkObjs xs ys
  flush
checkObjs _ _ = throw CheckError

checkMaybe ::
  (a -> a -> Code IO ()) ->
  Cell IO (Maybe a) -> Cell IO (Maybe a) -> Code IO ()
checkMaybe check x y = do
  x1 <- cellGet x
  y1 <- cellGet y
  case (x1, y1) of
    (Just x2, Just y2) -> do
      check x2 y2
      flush
    (Nothing, y2@Just{}) -> do
      cellPut x (pure y2)
      flush
    (x2@Just{}, Nothing) -> do
      cellPut y (pure x2)
      flush
    (Nothing, Nothing) ->
      pure ()

checkObj :: Obj IO -> Obj IO -> Code IO ()
checkObj ObjObj ObjObj = pure ()
checkObj (ObjArr leftArr) (ObjArr rightArr) = do
  checkArr leftArr rightArr
  flush
--  IDEA: Unify 'Obj' with 'Arr' from unit?
checkObj _ _ = throw CheckError

checkArr :: Arr IO -> Arr IO -> Code IO ()
checkArr
  (Arr leftInput leftOutput)
  (Arr rightInput rightOutput) = do
  checkMaybe checkObjs leftInput rightInput
  flush
  checkMaybe checkObjs leftOutput rightOutput
  flush

checkName :: Checked IO Scope -> Name -> Code IO (Arr IO)
checkName scope name = case Map.lookup name scope of
  Lazy.Just arr -> pure arr
  Lazy.Nothing -> throw CheckError

----------------------------------------------------------------
--  Compilation
----------------------------------------------------------------

data CompileError
  = CompileError
  deriving anyclass (Exception)
  deriving stock (Generic, Show)

type CompileScope = Map Name (Cell IO Value)

programCompile :: ProgramChecked -> ProgramCompiled
programCompile = compileBlock (mempty :: CompileScope)

compileBlock :: CompileScope -> Checked IO Block -> Code IO Value
compileBlock scope0 block = do
  scope1 <- fmap Map do
    for (Map.toList block.scope) compileBinding
  let
    -- IDEA: Allow shadowing to be a warning.
    scope2 = Map.unionWithKey
      (\key _old _new ->
        Prelude.error ("shadowed variable: " <> show key))
      scope0
      scope1
  compileExpression scope2 block.body

compileBinding ::
  (Name, Arr IO) ->
  Code IO (Name, Cell IO Value)
compileBinding (name, _type) = do
  cell <- cellNew (throw Uninitialized)
  pure (name, cell)

compileTerm :: CompileScope -> Checked IO Term -> Code IO Value
compileTerm scope0 = \case
  TermName _object name -> compileName scope0 name
  TermVar _object name -> compileName scope0 name
  TermValue value -> pure value
  TermGroup _object body -> compileExpression scope0 body
  TermBlock _object block -> compileBlock scope0 block

compileExpression ::
  CompileScope -> Checked IO Expression -> Code IO Value
compileExpression scope0 expression = undefined

compileName :: CompileScope -> Name -> Code IO Value
compileName scope name = case Map.lookup name scope of
  Lazy.Just cell -> cellGet cell
  Lazy.Nothing -> throw CompileError

----------------------------------------------------------------
