{-# Language BlockArguments #-}
{-# Language DeriveAnyClass #-}
{-# Language DefaultSignatures #-}
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
{-# Language PolyKinds #-}
{-# Language QuantifiedConstraints #-}
{-# Language TupleSections #-}
{-# Language TypeFamilies #-}
{-# Language TypeFamilyDependencies #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}
{-# Language ViewPatterns #-}

{-# Options_ghc -Wno-unused-imports #-}

module Hap (module Hap) where

import Control.Arrow (Arrow)
import Control.Arrow qualified as Arrow
import Control.Category (Category, (.), (>>>))
import Control.Category qualified as Category
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
import Control.Exception (Exception(displayException, fromException, toException), SomeAsyncException(..), SomeException(..), finally, throwIO, try)
import Control.Exception qualified as Exception
import Control.Monad ((<=<), join, void, when)
import Control.Monad.Except (MonadError, ExceptT, throwError, tryError)
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
import Data.Strict.Tuple (type (:!:), pattern (:!:), fst, snd, uncurry)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.IO qualified as Text.IO
import Data.Text.Read qualified as Text.Read
import Data.Traversable (for)
import Data.Void (Void, absurd, vacuous)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Exts (IsString)
import GHC.Exts qualified
import GHC.Natural (Natural)
import Optics ((%~))
import Optics qualified
import Prelude hiding (Either(..), Maybe(..), Word, (.), error, exp, fst, id, lines, maybe, snd, uncurry)
import Prelude qualified
import Prelude qualified as Lazy (Either(..), Maybe(..), fst, snd, uncurry)
import Prettyprinter (Pretty(pretty), hcat, hsep, hang, sep, vcat, vsep)
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.Text qualified
import SDL (($=))
import SDL qualified
import System.Console.Haskeline qualified as Haskeline
import System.Console.Haskeline.IO qualified as Haskeline.IO
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPrint, hPutStrLn, stderr)
import System.Mem.Weak qualified as Weak
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
  world <- worldNew
  compiledPrograms <- codeRun world do
    loaded <- traverse programLoad settings.sources
    compileAll loaded

  let
    startInput = do
      codeRun world do
        for_ compiledPrograms void
      exitCode <- case settings.input of
        InputBatch ->
          pure ExitSuccess
        InputInteractive ->
          consoleRun messages world
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

compileOne ::
  ProgramLoaded ->
  Code ProgramCompiled
compileOne loaded = do
  tokenized <- (varNew . pure . programTokenize) loaded
  parsed <- (varNew . (programParse <=< varGet)) tokenized
  checked <- (varNew . (programCheck <=< varGet)) parsed
  compiled <- (fmap programCompile . varGet) checked
  pure compiled

compileAll ::
  Seq ProgramLoaded ->
  Code (Seq ProgramCompiled)
compileAll loaded = do
  tokenized <- traverse (varNew . pure . programTokenize) loaded
  parsed <- traverse (varNew . (programParse <=< varGet)) tokenized
  checked <- traverse (varNew . (programCheck <=< varGet)) parsed
  compiled <- traverse (fmap programCompile . varGet) checked
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
    exitCode : _ -> throwError exitCode
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
    throwError exitCode

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
--  Reactive Vars
----------------------------------------------------------------

newtype Error
  = Error {
    message :: Doc
  }
  deriving stock (Show)

instance Exception Error where
  displayException = Text.unpack . prettyText . (.message)

instance IsString Error where
  fromString = Error . GHC.Exts.fromString

newtype Code a
  = Code {
    run :: World -> IO (Ans a)
  }

instance Debug (Code a) where
  debug _ = "⟨code⟩"

instance Functor Code where
  fmap f code = Code (fmap (fmap f) . code.run)

instance Applicative Code where

  pure = Code . const . pure . AnsNow . Right

  codeF <*> codeX = Code \world -> do
    ansF <- codeF.run world
    case ansF of

      --  The function is ready.
      AnsNow (Right f) -> do
        ansX <- codeX.run world
        case ansX of

          --  The input is ready: call.
          AnsNow (Right x) ->
            pure (AnsNow (Right (f x)))

          --  The input has failed: rethrow.
          AnsNow (Left e) ->
            pure (AnsNow (Left e))

          --  Awaiting input: call when the input is ready.
          AnsGet getX ->
            pure (AnsGet (fmap f getX))

      --  The function has failed: rethrow.
      AnsNow (Left e) ->
        pure (AnsNow (Left e))

      --  Awaiting the function.
      AnsGet getF -> do
        ansX <- codeX.run world
        case ansX of

          --  The input is ready:
          --  call when the function is also ready.
          AnsNow (Right x) ->
            pure (AnsGet (fmap ($ x) getF))

          --  The input has failed:
          --  rethrow after the function is ready.
          AnsNow (Left e)
            | Get forF _ <- getF ->
              (pure . AnsGet . Get forF . UseBind . const)
                (throwError e)

          --  Awaiting input.
          AnsGet (Get forX useX) -> case getF of

            --  The function is known:
            --  call when the function and input are ready.
            Get _forF (UseVar varF) ->
              getApply varF

            --  The function is unknown:
            --  make a variable to track it.
            Get forF useF -> do
              varF <- varNewUninitialized world
              let put = Put (useRun useF =<< varGet forF) varF
              varListen forF put
              getApply varF

            where
              getApply varF =
                (pure . AnsGet . Get forX . UseBind)
                  \x -> varGet varF <*> useRun useX x

throwUninitialized ::
  (MonadError SomeException m) =>
  m bottom
throwUninitialized =
  throwError (toException (Error "uninitialized"))

throwCycle ::
  (MonadError SomeException m) =>
  m bottom
throwCycle =
  throwError (toException (Error "cycle"))

useRun :: Use a b -> a -> Code b
useRun = \case
  UseVar varB -> \_a -> varGet varB
  UseBind needA -> \a -> needA a

putRun :: Put -> Code ()
putRun (Put code var) = varPutResult var . Right =<< code

instance Monad Code where
  codeX >>= codeF = Code \world -> do
    ansX <- codeX.run world
    case ansX of
      AnsNow (Right x) ->
        (codeF x).run world
      AnsNow (Left e) ->
        pure (AnsNow (Left e))
      AnsGet (Get forX useX) ->
        pure (AnsGet (Get forX (useX >>> UseBind codeF)))

instance MonadReader World Code where
  ask = Code (pure . AnsNow . Right)
  local f code = Code (code.run . f)

instance MonadError SomeException Code where
  throwError error =
    Code \_world -> throwIO error
  catchError code handler =
    Code \world -> do
      results <- try (code.run world)
      case results of
        Lazy.Left error -> do
          (handler error).run world
        Lazy.Right result -> do
          pure result

instance MonadIO Code where
  liftIO = Code . const . fmap (AnsNow . Right)

data World
  = World {
    next :: !(TVar Id),
    heap :: !(TVar Heap),
    queue :: !(TQueue Put)
  }
  deriving stock (Generic)

worldSnap :: World -> STM WorldSnap
worldSnap world = do
  next <- readTVar world.next
  heap <- heapSnap =<< readTVar world.heap
  --  TODO: queue
  pure WorldSnap { next, heap }

heapSnap :: Heap -> STM HeapSnap
heapSnap heap =
  fmap IdMap (traverse (someTraverse varSnap) heap.map)

varSnap :: Var a -> STM (VarSnap a)
varSnap var = do
  store <- readTVar var.store
  pure VarSnap { id = var.id, store }

data WorldSnap
  = WorldSnap {
    next :: !Id,
    heap :: !HeapSnap
    --  TODO: queue
  }
  deriving stock (Generic)

instance Debug WorldSnap where
  debug world =
    vsep [
      "world snapshot",
      hsep [
        "*",
        sep ["next:", debug world.next]
      ],
      hsep [
        "*",
        vsep [
          "heap:",
          hang 4 (debug world.heap)
        ]
      ]
    ]

type Heap =
  IdMap (Some Var)

type HeapSnap =
  IdMap (Some VarSnap)

data Ans a
  = AnsNow !(Result a)
  | AnsGet !(Get a)
  deriving stock (Functor)

data Get a where
  Get :: !(Var a) -> !(Use a b) -> Get b

data Put where
  Put :: !(Code b) -> !(Var b) -> Put

instance Functor Get where
  fmap f (Get before after) = Get before (Arrow.arr f . after)

data Use a b where
  UseVar :: !(Var b) -> Use a b
  UseBind :: !(a -> Code b) -> Use a b

instance Functor (Use a) where
  fmap f = \case
    UseVar x -> UseBind (const (fmap f (varGet x)))
    UseBind g -> UseBind (fmap f . g)

instance Category Use where
  id = Arrow.arr Category.id
  UseVar  y . UseVar  x = UseBind (const (varGet y) <=< const (varGet x))
  UseVar  y . UseBind f = UseBind (const (varGet y) <=< f)
  UseBind g . UseVar  x = UseBind (g <=< const (varGet x))
  UseBind g . UseBind f = UseBind (g <=< f)

instance Arrow Use where
  arr f = UseBind (pure . f)
  f *** g = UseBind \(x, y) -> liftA2 (,) (useRun f x) (useRun g y)

data Var a =
  Var {
    id :: Id,
    code :: !(TVar (Code a)),
    store :: !(TVar (Store a)),
    getters :: !(TVar [Put])
  }
  deriving stock (Generic)

instance Debug (Var a) where
  debug var = debug var.id

data VarSnap a =
  VarSnap {
    id :: Id,
    --  TODO: code
    store :: !(Store a)
    --  TODO: getters
  }
  deriving stock (Generic, Show)

instance (Debug a) => Debug (VarSnap a) where
  debug var =
    vsep [
      hsep ["*", sep [hcat ["id", ":"], debug var.id]],
      hsep ["*", sep [hcat ["store", ":"], debug var.store]]
    ]

class Debug a where
  debug :: a -> Doc
  --  default debug :: (Pretty a) => a -> Doc
  --  debug = pretty

dbg :: (Debug a) => a -> Pretty.Doc ann
dbg = vacuous . debug

instance Debug Int where
  debug = pretty

instance Debug Text where
  debug = pretty

instance Debug () where
  debug = pretty

instance (Debug a, Debug b) => Debug (a, b) where
  debug (a, b) = sep [hcat [debug a, ","], debug b]

instance (Debug a) => Debug (Maybe a) where
  debug = debug . Foldable.toList

instance (Debug a) => Debug [a] where
  debug = pretty . fmap (prettyText . debug)

instance (Debug a) => Debug (Seq a) where
  debug = debug . Foldable.toList

instance (Debug k, Debug v) => Debug (Map k v) where
  debug = debug . Map.toList

instance Debug (a -> b) where
  debug _ = "⟨function⟩"

data Some f where
  Some :: (Debug a) => !(f a) -> Some f

instance
  (
    forall x. (Debug x) => Debug (f x)
  ) => Debug (Some f) where
  debug (Some f) = debug f

someMap ::
  (forall x. f x -> g x) ->
  Some f ->
  Some g
someMap n (Some f) = Some (n f)

someTraverse ::
  (Functor m) =>
  (forall x. f x -> m (g x)) ->
  Some f ->
  m (Some g)
someTraverse n (Some f) = fmap Some (n f)

type Result = Either SomeException

data Store a
  = StoreFull !(Result a)
  | StoreEmpty
  | StoreFilling
  deriving stock (Generic, Show)

instance (Debug a) => Debug (Store a) where
  debug = \case
    StoreFull (Right result) ->
      sep [hcat ["full", ":"], debug result]
    StoreFull (Left error) ->
      sep [
        hcat ["full", ":"],
        debug (Text (displayException error))
      ]
    StoreEmpty ->
      "empty"
    StoreFilling ->
      "filling"

data Cache a
  --  IDEA: Split 'CacheFull' to save an indirection.
  = CacheFull !(Result a)
  | CacheEmpty
  | CacheFilling
  deriving stock (Generic, Show)

interpret :: Text -> IO (Maybe Value)
interpret text = do
  world <- worldNew
  let timeout_s = 2
  fmap toStrict do
    Timeout.timeout (micro timeout_s) do
      codeRun world do
        compiled <- compileOne text
        compiled

micro :: (Num a) => a -> a
micro = (* 1e6)

codeRun :: (Debug a) => World -> Code a -> IO a
codeRun world code0 = do
  resultVar <- varNewUninitialized world
  results <- loop [Put code0 resultVar]
  case results of
    Right [] -> do
      store <- atomically (readTVar resultVar.store)
      case store of
        StoreFull (Right result) -> do
          pure result
        StoreFull (Left error) -> do
          throw error
        StoreEmpty -> throw (Error "result empty (not begun)")
        StoreFilling -> throw (Error "result empty (not done)")
    Left error -> do
      throw error
    Right _queue -> throw (Error "ended with work left")

  where

    loop :: [Put] -> IO (Result [Put])
    loop (Put code var : queue0) = do
      answers <- try (code.run world)
      case answers of
        Lazy.Left error
          | Lazy.Just SomeAsyncException{} <-
              fromException error ->
            throw error
          | otherwise -> do
            pure (Left error)
        Lazy.Right (AnsNow result) -> do
          _ans <- (varPutResult var result).run world
          pure case result of
            Left error -> Left error
            Right{} -> Right queue0
        Lazy.Right (AnsGet (Get forResult useResult)) -> do
          varListen forResult
            (Put (useRun useResult =<< varGet forResult) var)
          loop queue0
    loop [] = do
      queue <- atomically do
        flushTQueue world.queue
      if null queue then pure (Right []) else loop queue

worldNew :: IO World
worldNew = do
  next <- newTVarIO (Id 0)
  heap <- heapNew
  queue <- newTQueueIO
  pure World { next, heap, queue }

heapNew :: IO (TVar Heap)
heapNew = newTVarIO idMapEmpty

flush :: Code ()
flush = do
  queue <- Reader.asks (.queue)
  actions <- atomically do
    flushTQueue queue
  for_ actions putRun

varGet ::
  Var a ->
  Code a
varGet var = (join . atomically) do
  store <- readTVar var.store
  case store of
    StoreFull (Right value) -> pure do
      pure value
    StoreFull (Left error) -> pure do
      throwError error
    StoreEmpty -> do
      writeTVar var.store StoreFilling
      code <- readTVar var.code
      pure do
        result <- tryError code
        case result of
          Lazy.Left error@(fromException -> Lazy.Just (_ :: Error)) -> do
            atomically do
              writeTVar var.store (StoreFull (Left error))
            throwError error
          Lazy.Right value -> do
            atomically do
              writeTVar var.store (StoreFull (Right value))
            pure value
    StoreFilling -> pure do
      throwCycle

varPutResult ::
  Var a ->
  Result a ->
  Code ()
varPutResult var result = do
  queue <- Reader.asks (.queue)
  -- Reset cached storage to full and notify readers.
  atomically do
    writeTVar var.store (StoreFull result)
    writeTVar var.code (Code (\_world -> pure (AnsNow result)))
    getters <- readTVar var.getters
    for_ getters (writeTQueue queue)

varPutLazy ::
  Var a ->
  Code a ->
  Code ()
varPutLazy var code = do
  queue <- Reader.asks (.queue)
  -- Reset cached storage to empty and notify readers.
  atomically do
    writeTVar var.store StoreEmpty
    writeTVar var.code code
    getters <- readTVar var.getters
    for_ getters (writeTQueue queue)

varOf ::
  (Debug a) =>
  a ->
  Code (Var a)
varOf = varNew . pure

varNew ::
  (Debug a) =>
  Code a ->
  Code (Var a)
varNew code = do
  world <- Reader.ask
  liftIO do
    varNewFrom world code

varNewUninitialized :: (Debug a) => World -> IO (Var a)
varNewUninitialized world = varNewFrom world throwUninitialized

varNewFrom ::
  (Debug a) =>
  World ->
  Code a ->
  IO (Var a)
varNewFrom world code0 = do
  id <- idNew world.next
  code <- newTVarIO code0
  getters <- newTVarIO []
  store <- newTVarIO StoreEmpty
  let
    var =
      Var {
        id,
        code,
        getters,
        store
      }
  atomically do
    modifyTVar' world.heap (idMapInsert id (Some var))
  pure var

varListen :: Var a -> Put -> IO ()
varListen var put = atomically do
  modifyTVar' var.getters (put :)

----------------------------------------------------------------
--  Var IDs
----------------------------------------------------------------

newtype Id = Id { number :: Nat }
  deriving newtype (Enum)
  deriving stock (Generic, Show)

instance Debug Id where
  debug id = "#" <> pretty id.number

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

instance (Debug a) => Debug (IdMap a) where
  debug ids =
    vsep [
      hsep ["*", sep [hcat [debug k, ":"], hang 4 (debug v)]]
      | (k, v) <- IntMap.toList ids.map
    ]

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

idNew :: TVar Id -> IO Id
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
    messages :: !Messages,
    inputState :: !Haskeline.IO.InputState,
    world :: !World
  }

consoleRun :: Messages -> World -> IO ExitCode
consoleRun messages world = do
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
        Just CommandVars -> do
          world <- atomically . worldSnap =<<
            Reader.asks (.world)
          consoleOutput (prettyText (debug world))
          consoleLoop
        Nothing -> do
          consoleOutput "unknown command"
          consoleLoop
      Lazy.Nothing -> do
        world <- Reader.asks (.world)
        results <- fmap toStrict do
          liftIO do
            try @SomeException do
              let timeout_s = 2
              fmap toStrict do
                Timeout.timeout (micro timeout_s) do
                  codeRun world do
                    join (compileOne line)
        consoleOutput case results of
          Right (Just result) -> Text (show result)
          Right Nothing -> "timed out"
          Left error ->
            Text (displayException error)
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
  | CommandVars
  deriving stock (Generic, Show)

parseCommand :: Text -> Maybe Command
parseCommand input0 = case Text.strip input0 of
  "quit" -> Just CommandQuit
  "vars" -> Just CommandVars
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

type Doc = Pretty.Doc Void

prettyText :: Doc -> Text
prettyText =
  Prettyprinter.Render.Text.renderStrict .
  Pretty.layoutPretty Pretty.defaultLayoutOptions

----------------------------------------------------------------
--  Programs
----------------------------------------------------------------

type ProgramLoaded = Text
type ProgramTokenized = Tokens
type ProgramParsed = Parsed Block
type ProgramChecked = Checked Block
type ProgramCompiled = Code Value

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

instance Debug Token where
  debug = \case
    TokenWord word ->
      pretty word
    TokenKeyword keyword ->
      hcat [pretty CharFullStop, pretty keyword]
    TokenText text ->
      hcat [
        pretty CharQuotationMark,
        pretty text,
        pretty CharQuotationMark
      ]
    TokenGroupBegin ->
      pretty CharLeftParenthesis
    TokenGroupEnd ->
      pretty CharRightParenthesis
    TokenBlockBegin ->
      pretty CharLeftCurlyBracket
    TokenBlockEnd ->
      pretty CharRightCurlyBracket

newtype Expression annotation where
  Expression :: { terms :: Terms a } -> Expression a
  deriving stock (Generic, Show)

instance (Debug a) => Debug (Expression a) where
  debug exp =
    hcat [
      pretty CharLeftParenthesis,
      sep (fmap debug (Foldable.toList exp.terms)),
      pretty CharRightParenthesis
    ]

type Terms a = Seq (Term a)

data Term annotation where
  TermName :: !a -> !Name -> Term a
  TermVar :: !a -> !Name -> Term a
  TermValue :: !Value -> Term a
  TermGroup :: !a -> !(Expression a) -> Term a
  TermBlock :: !a -> !(Block a) -> Term a
  deriving stock (Generic, Show)

instance (Debug a) => Debug (Term a) where
  debug = \case
    TermName _ann name ->
      debug name
    TermVar _ann name ->
      sep [
        hcat [pretty CharFullStop, pretty KeywordVar],
        debug name
      ]
    TermValue value ->
      debug value
    TermGroup _ann exp ->
      debug exp
    TermBlock _ann block ->
      debug block

type Obj :: Type
data Obj
  = ObjObj
  | ObjArr !Arr
  deriving stock (Generic)

instance Debug Obj where
  debug = \case
    ObjObj -> "*"
    ObjArr arr -> debug arr

type Objs :: Type
data Objs
  = ObjsNil
  | ObjsCons
    !(Var (Maybe Obj))
    !(Var (Maybe Objs))
  deriving stock (Generic)

instance Debug Objs where
  debug = \case
    ObjsNil -> "·"
    ObjsCons o os -> sep [hcat [debug o, ","], debug os]

type Arr :: Type
data Arr
  = Arr {
    input, output :: !(Var (Maybe Objs))
  }
  deriving stock (Generic)

instance Debug Arr where
  debug arr =
    sep [hsep [debug arr.input, "->"], debug arr.output]

data Value
  = ValueNumber !Number
  | ValueText !Text
  deriving stock (Generic, Show)

instance Debug Value where
  debug = Pretty.viaShow

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

instance (Debug a) => Debug (Block a) where
  debug block =
    sep [
      "[", debug block.scope, "]",
      "{", debug block.body, "}"
    ]

type Scope = Map Name

newtype Name =
  Name { spelling :: Text }
  deriving stock (Eq, Generic, Ord, Show)

instance Debug Name where
  debug = pretty . (.spelling)

type Number = Integer

programLoad :: Source -> Code ProgramLoaded
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

programParse ::
  ProgramTokenized ->
  Code ProgramParsed
programParse = parseAllTerms

parseAllTerms ::
  (MonadError SomeException m) =>
  Tokens ->
  m ProgramParsed
parseAllTerms tokens0
  | null tokens1 =
    pure (Block scope (Expression (Seq terms)))
  | otherwise =
    throwError (toException (Error "missing end of input"))
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
  (MonadError SomeException m) =>
  Tokens ->
  m (ParseResult (Parsed Term))
parseOneTerm = \case
  [] -> throwError (toException (Error "missing term"))
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
      | otherwise ->
        throwError (toException (Error "missing name"))
    TokenKeyword keyword ->
      throwError (toException (Error "unknown keyword"))
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
        throwError (toException (Error "missing group end"))
      where
        (terms, sites, tokens2) = parseManyTerms tokens1
    TokenGroupEnd ->
      throwError (toException (Error "unmatched group end"))
    TokenBlockBegin -> case tokens2 of
      TokenBlockEnd : tokens3 ->
        pure (block, mempty, tokens3)
        where
          block = TermBlock ()
            (Block scope (Expression (Seq terms)))
      _ ->
        throwError (toException (Error "missing block end"))
      where
        (terms, sites, tokens2) = parseManyTerms tokens1
        scope = scopeFromSites sites
    TokenBlockEnd ->
      throwError (toException (Error "unmatched block end"))

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

type Checked :: (Type -> Type) -> Type
type Checked f = f Arr

type WithArr = (:!:) Arr

programCheck :: ProgramParsed -> Code ProgramChecked
programCheck =
  fmap snd . checkBlock (mempty :: Checked Scope)

checkBlock ::
  Checked Scope ->
  Parsed Block ->
  Code (WithArr (Checked Block))
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

checkBinding :: (Name, ()) -> Code (Name, Arr)
checkBinding (name, ()) = do
  arr <- Arr
    <$> varOf Nothing
    <*> varOf Nothing
  pure (name, arr)

--  IDEA: Memoize to avoid allocating a lot of new vars.
checkTerm ::
  Checked Scope ->
  Parsed Term ->
  Code (WithArr (Checked Term))
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
      obj <- varOf (Just ObjObj)
      input <- varOf Nothing
      output <- varOf (Just (ObjsCons obj input))
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
      obj <- varOf (Just (ObjArr arr0))
      input <- varOf Nothing
      output <- varOf (Just (ObjsCons obj input))
      pure Arr { input, output }
    pure (arr1 :!: TermBlock arr1 block1)

checkExpression ::
  Checked Scope ->
  Parsed Expression ->
  Code (WithArr (Checked Expression))
checkExpression scope0 expression = do
  terms1 <- for expression.terms (checkTerm scope0)
  flush
  identityArr <- do
    objs <- varOf Nothing
    pure (Arr objs objs)
  arr <- (`State.execStateT` identityArr) do
    for_ terms1 \(producerArr :!: _producer) -> do
      consumerArr <- State.get
      composedArr <- lift do
        checkCompose consumerArr producerArr
      State.put composedArr
  flush
  pure (arr :!: Expression (fmap snd terms1))

checkCompose :: Arr -> Arr -> Code Arr
checkCompose consumer producer = do
  input0 <- varGet consumer.input
  output0 <- varGet producer.output
  for_ ((liftA2 (,) `on` toLazy) input0 output0)
    (Lazy.uncurry checkObjs)
  flush
  pure
    Arr {
      input = producer.input,
      output = consumer.output
    }

checkObjs :: Objs -> Objs -> Code ()
checkObjs ObjsNil ObjsNil = pure ()
checkObjs (ObjsCons x xs) (ObjsCons y ys) = do
  checkMaybe checkObj x y
  flush
  checkMaybe checkObjs xs ys
  flush
checkObjs _ _ =
  throwError (toException (Error "mismatched objects"))

checkMaybe ::
  (a -> a -> Code ()) ->
  Var (Maybe a) ->
  Var (Maybe a) ->
  Code ()
checkMaybe check x y = do
  x1 <- varGet x
  y1 <- varGet y
  case (x1, y1) of
    (Just x2, Just y2) -> do
      check x2 y2
      flush
    (Nothing, y2@Just{}) -> do
      varPutResult x (Right y2)
      flush
    (x2@Just{}, Nothing) -> do
      varPutResult y (Right x2)
      flush
    (Nothing, Nothing) ->
      pure ()

checkObj :: Obj -> Obj -> Code ()
checkObj ObjObj ObjObj = pure ()
checkObj (ObjArr leftArr) (ObjArr rightArr) = do
  checkArr leftArr rightArr
  flush
--  IDEA: Unify 'Obj' with 'Arr' from unit?
checkObj _ _ =
  throwError (toException (Error "object and arrow"))

checkArr :: Arr -> Arr -> Code ()
checkArr
  (Arr leftInput leftOutput)
  (Arr rightInput rightOutput) = do
  checkMaybe checkObjs leftInput rightInput
  flush
  checkMaybe checkObjs leftOutput rightOutput
  flush

checkName :: Checked Scope -> Name -> Code Arr
checkName scope name = case Map.lookup name scope of
  Lazy.Just arr -> pure arr
  Lazy.Nothing ->
    throwError (toException (Error "missing name"))

----------------------------------------------------------------
--  Compilation
----------------------------------------------------------------

type CompileScope = Map Name (Var Value)

programCompile :: ProgramChecked -> ProgramCompiled
programCompile = compileBlock (mempty :: CompileScope)

compileBlock :: CompileScope -> Checked Block -> Code Value
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
  (Name, Arr) ->
  Code (Name, Var Value)
compileBinding (name, _type) = do
  var <- varNew do
    throwError (toException (Error "uninitialized"))
  pure (name, var)

compileTerm :: CompileScope -> Checked Term -> Code Value
compileTerm scope0 = \case
  TermName _object name -> compileName scope0 name
  TermVar _object name -> compileName scope0 name
  TermValue value -> pure value
  TermGroup _object body -> compileExpression scope0 body
  TermBlock _object block -> compileBlock scope0 block

compileExpression ::
  CompileScope ->
  Checked Expression ->
  Code Value
compileExpression scope0 expression = undefined

compileName ::
  CompileScope ->
  Name ->
  Code Value
compileName scope name = case Map.lookup name scope of
  Lazy.Just var -> varGet var
  Lazy.Nothing ->
    throwError (toException (Error "missing name"))

----------------------------------------------------------------
