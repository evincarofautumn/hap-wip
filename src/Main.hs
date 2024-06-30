{-# Language BlockArguments #-}
{-# Language DeriveAnyClass #-}
{-# Language DerivingStrategies #-}
{-# Language DerivingVia #-}
{-# Language DuplicateRecordFields #-}
{-# Language LambdaCase #-}
{-# Language NoFieldSelectors #-}
{-# Language OverloadedLabels #-}
{-# Language OverloadedRecordDot #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}

module Main where

import Control.Concurrent (forkIO, forkOS)
import Control.Concurrent.MVar (MVar, isEmptyMVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.MVar qualified as MVar
import Control.Concurrent.STM.TBQueue (TBQueue, flushTBQueue)
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM.TChan qualified as TChan
import Control.Concurrent.STM.TMVar (TMVar, isEmptyTMVar, newEmptyTMVar, putTMVar, takeTMVar)
import Control.Concurrent.STM.TMVar qualified as TMVar
import Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVar, stateTVar, writeTVar)
import Control.Concurrent.STM.TVar qualified as TVar
import Control.Exception (Exception, SomeException(SomeException), finally, throwIO, try)
import Control.Monad ((<=<), join, void, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Reader qualified as Reader
import Control.Monad.STM (STM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (MonadWriter, WriterT)
import Control.Monad.Writer qualified as Writer
import Data.ByteString qualified as ByteString
import Data.Coerce (coerce)
import Data.DList (DList)
import Data.DList qualified as DList
import Data.Foldable (for_)
import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.List (partition)
import Data.Sequence (Seq((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Strict.Either (Either(Left, Right))
import Data.Strict.Maybe (Maybe(Nothing, Just))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Traversable (for)
import Data.Void (Void)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Hap qualified
import Optics ((%~))
import Optics qualified
import Prelude hiding (Either(..), Maybe(..), Word, error, id, lines)
import Prelude qualified
import Prelude qualified as Lazy (Either(..), Maybe(..))
import Prettyprinter (Pretty(pretty))
import Prettyprinter qualified
import Prettyprinter.Render.Text qualified
import SDL (($=))
import SDL qualified
import System.Console.Haskeline qualified as Haskeline
import System.Console.Haskeline.IO qualified as Haskeline.IO
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPrint, hPutStrLn, stderr)
import System.Mem.Weak (Weak, mkWeakPtr)

main :: IO ()
main = start defaultSettings

debug :: String -> IO ()
--  debug = hPutStrLn stderr
debug _ = pure ()

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

start :: Settings -> IO ()
start settings = do

  messages <- messagesNew
  world <- worldNew
  compiledPrograms <- hapRun
    world
    (compileAll settings.sources)

  let
    startInput = (void . forkIO) do
      debug "input thread"
      debug "running programs"
      hapRun world do
        for_ compiledPrograms void
      debug "running input"
      exitCode <- case settings.input of
        InputBatch -> do
          debug "batch mode"
          pure ExitSuccess
        InputInteractive -> do
          debug "interactive mode"
          consoleRun world
      debug "done input thread"
      messagesInterrupt messages exitCode

    startOutput = do
      debug "output thread"
      case settings.output of
        OutputGraphics -> do
          debug "graphics mode"
          graphicsRun messages startInput
        OutputText -> do
          debug "text mode"
          startInput
          messagesLoop
            messages
            (\_interrupt -> pure ())
            (\_request -> pure ())

  debug "starting output"
  startOutput

  debug "waiting"
  exitWith =<< messagesWait messages

compileOne :: Source -> Code ProgramCompiled
compileOne source = do
  loaded <- (cellNew . programLoad) source
  parsed <- (cellNew . (programParse <=< cellGet)) loaded
  compiled <- (cellNew . (programCompile <=< cellGet)) parsed
  results <- cellGet compiled
  pure results

compileAll :: Sources -> Code (Seq ProgramCompiled)
compileAll sources = do
  loaded <- traverse (cellNew . programLoad) sources
  parsed <- traverse (cellNew . (programParse <=< cellGet)) loaded
  compiled <- traverse (cellNew . (programCompile <=< cellGet)) parsed
  results <- traverse cellGet compiled
  pure results

----------------------------------------------------------------
--  Output Thread
----------------------------------------------------------------

data Messages =
  Messages {
    interruptions :: !(MVar Interruption),
    requests :: !(TBQueue Request)
    --  requests :: !(TChan Request)
  }
  deriving stock (Generic)

type Interruption =
  ExitCode

data Request
  = RequestOutput !Text
  | RequestGraphicsClear
  | RequestGraphicsBackgroundSet
    !(Word8) !(Word8) !(Word8) !(Word8)
  | RequestGraphicsPresent
  deriving stock (Generic, Show)

data Graphics =
  Graphics {
    window :: !SDL.Window,
    renderer :: !SDL.Renderer
  }
  deriving stock (Generic)

messagesNew :: IO Messages
messagesNew = do
  interruptions <- newEmptyMVar
  --  requests <- newTChanIO
  requests <- newTBQueueIO requestsCapacity
  pure
    Messages {
      interruptions,
      requests
    }

requestsCapacity :: Natural
requestsCapacity = 1024

graphicsNew :: IO Graphics
graphicsNew = do
  debug "starting graphics"
  SDL.initializeAll
  debug "creating window"
  window <- SDL.createWindow "Hap" SDL.defaultWindow
  let firstSupportedDriver = -1
  debug "creating renderer"
  renderer <- SDL.createRenderer
    window
    firstSupportedDriver
    SDL.defaultRenderer
  debug "done starting graphics"
  pure
    Graphics {
      window,
      renderer
    }

graphicsRun :: Messages -> IO () -> IO ()
graphicsRun messages inited = do
  debug "running graphics"
  graphics <- graphicsNew
  debug "starting graphics loop"
  inited
  messagesLoop
    messages
    (graphicsHandleEvents graphics)
    (graphicsHandleRequest graphics)
  graphicsEnd graphics
  debug "done running graphics"

graphicsHandleEvents ::
  Graphics -> (Interruption -> IO ()) -> IO ()
graphicsHandleEvents graphics interrupt = do
  debug "polling for events"
  events <- SDL.pollEvents
  let
    (interruptions, messages) =
      partition isInterruption events
  if null interruptions
    then do
      debug "no interruptions"
      for_ messages (graphicsHandleEvent graphics)
    else do
      debug "interrupted"
      interrupt ExitSuccess

graphicsHandleEvent :: Graphics -> SDL.Event -> IO ()
graphicsHandleEvent graphics event = do
  debug (show event)
  case SDL.eventPayload event of
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

graphicsHandleRequest :: Graphics -> Request -> IO ()
graphicsHandleRequest _graphics request = do
  debug (show request)
  --  pure ()

messagesInterrupt ::
  (MonadIO m) => Messages -> Interruption -> m ()
messagesInterrupt messages =
  liftIO . putMVar messages.interruptions

messagesWait :: 
  (MonadIO m) => Messages -> m Interruption
messagesWait messages =
  liftIO (takeMVar messages.interruptions)

messagesLoop ::
  Messages ->
  ((Interruption -> IO ()) -> IO ()) ->
  (Request -> IO ()) ->
  IO ()
messagesLoop messages handleEvents handleRequest = do
  debug "message loop"
  loop
  debug "done message loop"
  where
    loop = do
      debug "checking for interruptions"
      uninterrupted <- {-atomically-}
        (isEmptyMVar messages.interruptions)
      debug "checked for interruptions"
      when uninterrupted do
        debug "handling events"
        handleEvents (messagesInterrupt messages)
        debug "handling requests"
        let
          --  flush =
          --    atomically (TChan.tryReadTChan messages.requests) >>= \case
          --      Lazy.Just request -> do
          --        handleRequest request
          --        flush
          --      Lazy.Nothing -> pure ()
          flush = do
            requests <- atomically (flushTBQueue messages.requests)
            for_ requests handleRequest
        -- flush
        debug "looping"
        loop

graphicsEnd :: Graphics -> IO ()
graphicsEnd graphics = do
  debug "stopping graphics"
  SDL.destroyWindow graphics.window
  debug "done stopping graphics"

isInterruption :: SDL.Event -> Bool
isInterruption event = case SDL.eventPayload event of
  SDL.KeyboardEvent keyboardEvent
    | let keyMotion = SDL.keyboardEventKeyMotion keyboardEvent,
      SDL.Pressed <- keyMotion,
      let keysym = SDL.keyboardEventKeysym keyboardEvent,
      SDL.KeycodeQ <- SDL.keysymKeycode keysym ->
      True
  SDL.QuitEvent ->
    True
  _ ->
    False

----------------------------------------------------------------
--  Reactive Cells
----------------------------------------------------------------

type Code = CodeT IO

newtype CodeT m a =
  CodeT {
    execute :: World -> m (a, Cells)
  }
  deriving
    (
      Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadReader World,
      MonadWriter Cells
    ) via (ReaderT World (WriterT Cells m))

type Cells =
  DList CellSome

data World =
  World {
    allocator :: !IdSource,
    heap :: !Heap
  }
  deriving stock (Generic)

type Heap =
  TVar (IdMap CellSome)

data CellSome where
  CellSome :: !(Cell a) -> CellSome

instance Show CellSome where
  showsPrec p CellSome{} =
    showParen (p > 10) (showString "CellSome _")

data CellWeak where
  CellWeak :: !(Weak (Cell a)) -> CellWeak

data Cell a =
  Cell {
    code :: !(TVar (Code a)),
    getters :: !(TVar (Seq CellWeak)),
    cache :: !(TVar (Cache a))
  }
  deriving stock (Generic)

data Cache a
  = CacheFull !(Result a)
  | CacheEmpty
  | CacheFilling

type Result =
  Either SomeException

hapRun :: World -> Code a -> IO a
hapRun world code = fmap fst (code.execute world)

worldNew :: IO World
worldNew = do
  allocator <- idSourceNew
  heap <- heapNew
  pure World { allocator, heap }

heapNew :: IO Heap
heapNew = newTVarIO idMapEmpty

cellGet :: Cell a -> Code a
cellGet cell = CodeT \world ->
  liftIO =<< atomically do
    cache <- readTVar cell.cache
    case cache of
      CacheFull (Right value) -> do
        pure (pure (value, DList [CellSome cell]))
      CacheFull (Left error) -> do
        pure (throwIO error)
      CacheEmpty -> do
        writeTVar cell.cache CacheFilling
        code <- readTVar cell.code
        pure do
          result <- try (code.execute world)
          case result of
            Lazy.Left error -> do
              atomically do
                writeTVar cell.cache (CacheFull (Left error))
              throwIO error
            Lazy.Right (value, putters) -> do
              getter <- cellWeakNew cell
              atomically do
                writeTVar cell.cache (CacheFull (Right value))
                for_ putters \(CellSome putter) ->
                  modifyTVar' putter.getters (getter :<|)
              pure (value, DList [CellSome cell])
      CacheFilling ->
        pure (throwIO (Cycle (CellSome cell)))

data Cycle = Cycle !CellSome
  deriving anyclass (Exception)
  deriving stock (Show)

cellNew :: Code a -> Code (Cell a)
cellNew code = do
  allocator <- Reader.asks (.allocator)
  id <- liftIO (idNew allocator)
  code <- newTVarIO code
  getters <- newTVarIO Seq.Empty
  cache <- newTVarIO CacheEmpty
  let
    cell =
      Cell {
        code,
        getters,
        cache
      }
  do
    heap <- Reader.asks (.heap)
    atomically (modifyTVar' heap (idMapInsert id (CellSome cell)))
  pure cell

cellWeakNew :: (MonadIO m) => Cell a -> m CellWeak
cellWeakNew cell =
  fmap CellWeak (liftIO (mkWeakPtr cell Lazy.Nothing))

----------------------------------------------------------------
--  Cell IDs
----------------------------------------------------------------

newtype Id = Id { number :: Nat }
  deriving newtype (Enum)
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

type IdSource = TVar Id

idSourceNew :: IO IdSource
idSourceNew = newTVarIO (Id 0)

idNew :: (MonadIO m) => IdSource -> m Id
idNew source = atomically do
  stateTVar source next
  where
    next id0 = (id0, id1)
      where
        !id1 = succ id0

----------------------------------------------------------------
--  Console
----------------------------------------------------------------

data Console =
  Console {
    inputState :: !Haskeline.IO.InputState,
    world :: !World
  }

consoleRun :: World -> IO ExitCode
consoleRun world = do
  --  Haskeline.runInputTBehavior
  --    Haskeline.preferTerm
  --    Haskeline.defaultSettings
  --    (Reader.runReaderT consoleLoop world)
  inputState <- Haskeline.IO.initializeInput
    Haskeline.defaultSettings
  let console = Console { inputState, world }
  Reader.runReaderT consoleLoop console
    `finally` Haskeline.IO.cancelInput inputState

type Consoled = ReaderT Console IO

consoleLoop :: Consoled ExitCode
consoleLoop = do
  let prompt = "> "
  lines <- consoleInput prompt
  case lines of
    Lazy.Nothing ->
      consoleBye
    Lazy.Just line -> case parseComment line of
      Lazy.Just comment -> case parseCommand comment of
        Just CommandQuit ->
          consoleBye
        Nothing -> do
          consoleOutput "unknown command"
          consoleLoop
      Lazy.Nothing -> do
        world <- Reader.asks (.world)
        result <- liftIO
          (hapRun world (join (compileOne (SourceText line))))
        -- (prettyText (pretty result))
        consoleOutput (Text (show result))
        consoleLoop

consoleBye :: Consoled ExitCode
consoleBye = do
  consoleOutput "Bye"
  pure ExitSuccess

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
-- Convenience Patterns
----------------------------------------------------------------

pattern DList :: [a] -> DList a
pattern DList list <- (DList.toList -> list)
  where
    DList list = DList.fromList list
{-# Complete DList #-}

pattern Text :: String -> Text
pattern Text string <- (Text.unpack -> string)
  where
    Text string = Text.pack string
{-# Complete Text #-}

----------------------------------------------------------------
--  MonadIO Wrappers
----------------------------------------------------------------

atomically :: (MonadIO m) => STM a -> m a
atomically = liftIO . atomically

newTBQueueIO :: (MonadIO m) => Natural -> m (TBQueue a)
newTBQueueIO = liftIO . TBQueue.newTBQueueIO

newTChanIO :: (MonadIO m) => m (TChan a)
newTChanIO = liftIO TChan.newTChanIO

newEmptyTMVarIO :: (MonadIO m) => m (TMVar a)
newEmptyTMVarIO = liftIO TMVar.newEmptyTMVarIO

newTVarIO :: (MonadIO m) => a -> m (TVar a)
newTVarIO = liftIO . TVar.newTVarIO

readTVarIO :: (MonadIO m) => TVar a -> m a
readTVarIO = liftIO . TVar.readTVarIO

throw :: (Exception e, MonadIO m) => e -> CodeT m z
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
type ProgramParsed = Expression
type ProgramCompiled = Code Value

newtype Expression = Expression { terms :: Terms }
  deriving stock (Generic, Show)

type Terms = Seq Term

data Term
  = TermName !Name
  | TermNumber !Number
  | TermText !Text
  | TermBlock !Bindings !Expression
  deriving stock (Generic, Show)

data Value
  = ValueNumber !Number
  | ValueText !Text
  deriving stock (Generic, Show)

type Bindings = Seq Binding

data Binding
  = Binding {
    name :: !Name,
    annotation :: !(Maybe Expression)
  }
  deriving stock (Generic, Show)

newtype Name =
  Name { spelling :: Text }
  deriving stock (Generic, Show)

type Number = Integer

data ParseError
  = ParseError
  deriving anyclass (Exception)
  deriving stock (Generic, Show)

programLoad :: Source -> Code ProgramLoaded
programLoad = \case
  SourcePath path ->
    fmap decodeUtf8 (liftIO (ByteString.readFile path))
  SourceText text ->
    pure text

programParse :: ProgramLoaded -> Code ProgramParsed
programParse _program = throw ParseError

data CompileError
  = CompileError
  deriving anyclass (Exception)
  deriving stock (Generic, Show)

programCompile :: ProgramParsed -> Code ProgramCompiled
programCompile _program = throw CompileError

----------------------------------------------------------------
