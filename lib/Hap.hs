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

import Prelude hiding
  (
    Either(..),
    Maybe(..),
    Word,
    (.),
    error,
    exp,
    fst,
    head,
    id,
    init,
    last,
    lines,
    maybe,
    snd,
    tail,
    uncurry,
  )
import Prelude qualified
import Prelude qualified as Lazy
  (
    Either(..),
    Maybe(..),
    fst,
    maybe,
    snd,
    uncurry,
  )

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
import Control.Monad ((<=<), (>=>), join, void, when)
import Control.Monad.Except (MonadError, ExceptT, throwError, tryError)
import Control.Monad.Except qualified as Error
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ReaderT(runReaderT))
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
import Data.Foldable (fold, for_, sequenceA_, toList, traverse_)
import Data.Foldable qualified as Foldable
import Data.Function ((&), on)
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.Kind (Constraint, Type)
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Semigroup (Last(Last, getLast))
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
import Prettyprinter (Pretty(pretty), hcat, hsep, hang, sep, vcat, vsep)
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.Text qualified
import SDL (($=))
import SDL qualified
import System.Console.Haskeline qualified as Haskeline
import System.Console.Haskeline.IO qualified as Haskeline.IO
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPrint, hPutStrLn, stderr)
import System.Mem.Weak (Weak)
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
  tokenized <- (varNewStrict . pure . programTokenize) loaded
  parsed <- (varNewStrict . (programParse <=< varGet)) tokenized
  compiled <- (programCompile <=< varGet) parsed
  pure compiled

compileAll ::
  Seq ProgramLoaded ->
  Code (Seq ProgramCompiled)
compileAll loaded = do
  tokenized <- traverse (varNewStrict . pure . programTokenize) loaded
  parsed <- traverse (varNewStrict . (programParse <=< varGet)) tokenized
  compiled <- traverse (programCompile <=< varGet) parsed
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

throwUnsolved ::
  (MonadError SomeException m) =>
  m bottom
throwUnsolved =
  throwError (toException (Error "unsolved"))

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
    scope :: !(IORef CompileScope),
    stack :: !(IORef [Var Value]),
    queue :: !(TQueue Put)
  }
  deriving stock (Generic)

stackPush :: Var Value -> Code ()
stackPush value = do
  stack <- Reader.asks (.stack)
  modifyIORef' stack (value :)

stackPop :: Code (Var Value)
stackPop = do
  stack <- Reader.asks (.stack)
  readIORef stack >>= \case
    [] -> throwError (toException (Error "underflow"))
    top : down -> do
      writeIORef stack down
      pure top

worldSnap :: (MonadIO m) => World -> m WorldSnap
worldSnap world = do
  next :!: heap0 <- atomically do
    liftA2 (:!:) (readTVar world.next) (readTVar world.heap)
  heap <- heapSnap heap0
  stack <- stackSnap =<< readIORef world.stack
  --  TODO: queue
  pure WorldSnap { next, heap, stack }

heapSnap :: (MonadIO m) => Heap -> m HeapSnap
heapSnap heap = do
  vars <- IntMap.traverseMaybeWithKey
    (const (liftIO . Weak.deRefWeak))
    heap.map
  fmap IdMap (atomically (traverse (someTraverse varSnap) vars))

stackSnap :: (MonadIO m) => [Var Value] -> m [VarSnap Value]
stackSnap = atomically . traverse varSnap

varSnap :: Var a -> STM (VarSnap a)
varSnap var = do
  store <- readTVar var.store
  pure VarSnap { id = var.id, store }

data WorldSnap
  = WorldSnap {
    next :: !Id,
    heap :: !HeapSnap,
    stack :: ![VarSnap Value]
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
      ],
      hsep [
        "*",
        vsep [
          "stack:",
          hang 4 (debug world.stack)
        ]
      ]
    ]

type Heap =
  IdMap (Weak (Some Var))

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
    id :: !Id,
    strat :: !Strat,
    code :: !(TVar (Code a)),
    store :: !(TVar (Store a)),
    getters :: !(TVar [Put])
  }
  deriving stock (Generic)

instance Debug (Var a) where
  debug var = debug var.id

data Strat
  = StratLazy
  | StratStrict

instance Debug Strat where
  debug = \case
    StratLazy -> "lazy"
    StratStrict -> "strict"

data VarSnap a =
  VarSnap {
    id :: !Id,
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

instance (Debug a) => Debug (Identity a) where
  debug = debug . runIdentity

instance (Debug a) => Debug (Maybe a) where
  debug = debug . each

instance (Debug a) => Debug [a] where
  debug = pretty . fmap (prettyText . debug)

instance (Debug a) => Debug (Seq a) where
  debug = debug . each

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

interpret :: Text -> IO (Maybe ())
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

codeTry ::
  (Debug a, MonadIO m) =>
  World ->
  Code a ->
  m (Result a)
codeTry world code = fmap (toStrict . join) do
  liftIO do
    try @SomeException do
      let timeout_s = 2
      fmap
        (Lazy.maybe
          (Lazy.Left (toException (Error "timed out")))
          Lazy.Right)
        do
          Timeout.timeout (micro timeout_s) do
            codeRun world code

codeTest :: (Debug a) => Code a -> IO a
codeTest code = do
  world <- worldNew
  codeRun world code

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
  scope <- IORef.newIORef (mempty :: CompileScope)
  stack <- IORef.newIORef []
  queue <- newTQueueIO
  pure World { next, heap, scope, stack, queue }

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
          Lazy.Left error -> do
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
varOf = varNew StratStrict . pure

varNew ::
  (Debug a) =>
  Strat ->
  Code a ->
  Code (Var a)
varNew strat code = do
  world <- Reader.ask
  liftIO do
    varNewFrom world strat code

varNewStrict ::
  (Debug a) =>
  Code a ->
  Code (Var a)
varNewStrict = varNew StratStrict

varNewLazy ::
  (Debug a) =>
  Code a ->
  Code (Var a)
varNewLazy = varNew StratLazy

varNewUninitialized :: (Debug a) => World -> IO (Var a)
varNewUninitialized world =
  varNewFrom world StratLazy throwUninitialized

varNewFrom ::
  (Debug a) =>
  World ->
  Strat ->
  Code a ->
  IO (Var a)
varNewFrom world strat code0 = do
  id <- idNew world.next
  code <- newTVarIO code0
  getters <- newTVarIO []
  store <- newTVarIO =<< case strat of
    StratLazy -> pure StoreEmpty
    StratStrict -> fmap StoreFull (codeTry world code0)
  let
    var =
      Var {
        id,
        strat,
        code,
        getters,
        store
      }
  let delete = modifyTVar' world.heap (idMapDelete id)
  weak <- Weak.mkWeakPtr (Some var)
    (Lazy.Just (atomically delete))
  let insert = modifyTVar' world.heap (idMapInsert id weak)
  atomically insert
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

idMapDelete :: Id -> IdMap a -> IdMap a
idMapDelete id ids =
  ids & #map %~
    IntMap.delete (fromIntegral id.number)

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
  runReaderT consoleLoop console
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
          world <- worldSnap =<< Reader.asks (.world)
          consoleOutput (prettyText (debug world))
          consoleLoop
        Nothing -> do
          consoleOutput "unknown command"
          consoleLoop
      Lazy.Nothing -> do
        world <- Reader.asks (.world)
        results <- codeTry world do
          join (compileOne line)
        consoleOutput case results of
          Right result -> Text (show result)
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
--  Renaming
----------------------------------------------------------------

type Nat = Prelude.Word

{-# Inline each #-}
each :: (Foldable t) => t a -> [a]
each = Foldable.toList

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

pattern CharComma :: Char
pattern CharComma = '\x002C'

pattern CharFullStop :: Char
pattern CharFullStop = '\x002E'

pattern CharSemicolon :: Char
pattern CharSemicolon = '\x003B'

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
  CharComma -> True
  CharSemicolon -> True
  CharLeftCurlyBracket -> True
  CharRightCurlyBracket -> True
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
pattern Seq list <- (each -> list)
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

modifyIORef' :: (MonadIO m) => IORef a -> (a -> a) -> m ()
modifyIORef' = fmap liftIO . IORef.modifyIORef

newTBQueueIO :: (MonadIO m) => Natural -> m (TBQueue a)
newTBQueueIO = liftIO . TBQueue.newTBQueueIO

newTChanIO :: (MonadIO m) => m (TChan a)
newTChanIO = liftIO TChan.newTChanIO

newTQueueIO :: (MonadIO m) => m (TQueue a)
newTQueueIO = liftIO TQueue.newTQueueIO

newTVarIO :: (MonadIO m) => a -> m (TVar a)
newTVarIO = liftIO . TVar.newTVarIO

readIORef :: (MonadIO m) => IORef a -> m a
readIORef = liftIO . IORef.readIORef

readTVarIO :: (MonadIO m) => TVar a -> m a
readTVarIO = liftIO . TVar.readTVarIO

throw :: (Exception e, MonadIO m) => e -> m z
throw = liftIO . throwIO

writeIORef :: (MonadIO m) => IORef a -> a -> m ()
writeIORef = fmap liftIO . IORef.writeIORef

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
type ProgramCompiled = Code ()

type Tokens = [Token]

data Token
  = TokenWord !Text
  | TokenKeyword !Text
  | TokenText !Text
  | TokenGroupBegin
  | TokenGroupEnd
  | TokenBlockBegin
  | TokenBlockEnd
  | TokenExpressionSeparator
  | TokenStatementSeparator
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
    TokenExpressionSeparator ->
      pretty CharComma
    TokenStatementSeparator ->
      pretty CharSemicolon

newtype Expression annotation where
  Expression :: { terms :: Terms a } -> Expression a
  deriving stock
    (
      Foldable,
      Functor,
      Generic,
      Show,
      Traversable
    )

type Expressions a = Seq (Expression a)

type Statement = Expression

instance (Debug a) => Debug (Expression a) where
  debug exp =
    hcat [
      pretty CharLeftParenthesis,
      sep (fmap debug (each exp.terms)),
      pretty CharRightParenthesis
    ]

type Terms a = Seq (Term a)

data Term annotation where
  TermName :: !a -> !Name -> Term a
  TermVar :: !a -> !Name -> Term a
  TermValue :: !Value -> Term a
  TermGroup :: !a -> !(Expression a) -> Term a
  TermBlock :: !a -> !(Block a) -> Term a
  deriving stock
    (
      Foldable,
      Functor,
      Generic,
      Show,
      Traversable
    )

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

type Obj :: (Type -> Type) -> Type
data Obj var
  = ObjObj
  | ObjArr !(Arr var)
  deriving stock (Generic)

instance
  (
    forall x. (Debug x) => Debug (var x)
  ) => Debug (Obj var) where
  debug = \case
    ObjObj -> "*"
    ObjArr arr -> debug arr

type Objs :: (Type -> Type) -> Type
data Objs var
  = ObjsNil
  | ObjsCons !(var (Obj var)) !(var (Objs var))
  deriving stock (Generic)

instance
  (
    forall x. (Debug x) => Debug (var x)
  ) => Debug (Objs var) where
  debug = \case
    ObjsNil -> "·"
    ObjsCons o os -> sep [hcat [debug o, ","], debug os]

type Arr :: (Type -> Type) -> Type
data Arr var
  = Arr {
    input, output :: !(var (Objs var))
  }
  deriving stock (Generic)

instance
  (
    forall x. (Debug x) => Debug (var x)
  ) => Debug (Arr var) where
  debug arr =
    sep [hsep [debug arr.input, "->"], debug arr.output]

data Value
  = ValueNone
  | ValueNumber !Number
  | ValueText !Text
  deriving stock (Generic, Show)

instance Debug Value where
  debug = \case
    ValueNone -> "none"
    ValueNumber number -> pretty number
    ValueText text ->
      hcat [
        pretty CharQuotationMark,
        pretty text,
        pretty CharQuotationMark
      ]

data Uninitialized
  = Uninitialized
  deriving anyclass (Exception)
  deriving stock (Generic, Show)

data Block annotation where
  Block ::
    {
      scope :: !(Scope a),
      body :: !(Expressions a)
    } -> Block a
  deriving stock
    (
      Foldable,
      Functor,
      Generic,
      Show,
      Traversable
    )

instance (Debug a) => Debug (Block a) where
  debug block =
    sep [
      "[", debug block.scope, "]",
      "{",
      sep [
        hcat [debug statement, ";"]
        | statement <- each block.body
      ],
      "}"
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
        CharComma -> Just TokenExpressionSeparator
        CharSemicolon -> Just TokenStatementSeparator
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
programParse = parseAllStatements

parseAllStatements ::
  (MonadError SomeException m) =>
  Tokens ->
  m ProgramParsed
parseAllStatements tokens0
  | null tokens1 =
    pure Block { scope, body = Seq statements }
  | otherwise =
    throwError (toException (Error "missing end of input"))
  where
    (statements, sites, tokens1) = parseManyStatements tokens0
    scope = scopeFromSites sites

--  IDEA: Handle duplicate bindings and annotations here.
scopeFromSites :: BindingSites -> Parsed Scope
scopeFromSites = void . (.union)

parseManyStatements :: Tokens -> ParseResult [Parsed Statement]
parseManyStatements tokens0
  | null statement.terms = ([], sites0, tokens1)
  | otherwise = case tokens1 of
    TokenStatementSeparator : tokens2 ->
      (statement : statements, sites0 <> sites1, tokens3)
      where
        ~(statements, sites1, tokens3) =
          parseManyStatements tokens2
    tokens2 ->
      (
        [statement],
        sites0,
        tokens2
      )
  where
    ~(statement, sites0, tokens1) = parseExpression tokens0

parseExpression :: Tokens -> ParseResult (Parsed Expression)
parseExpression tokens0 =
  (Expression (Seq terms), sites, tokens1)
  where
    ~(terms, sites, tokens1) = parseManyTerms tokens0

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
            (Block scope (Seq statements))
      _ ->
        throwError (toException (Error "missing block end"))
      where
        ~(statements, sites, tokens2) =
          parseManyStatements tokens1
        scope = scopeFromSites sites
    TokenBlockEnd ->
      throwError (toException (Error "unmatched block end"))
    TokenExpressionSeparator ->
      throwError (toException (Error "misplaced expression separator"))
    TokenStatementSeparator ->
      throwError (toException (Error "misplaced statement separator"))

parseNumber :: Text -> Maybe Number
parseNumber word
  | Lazy.Right (number, TextEmpty) <-
      Text.Read.signed Text.Read.decimal word =
    Just number
  | otherwise =
    Nothing

----------------------------------------------------------------
--  Compilation
----------------------------------------------------------------

type Compile :: Type -> Type
type Compile = ReaderT CompileScope Code

type CompileScope :: Type
type CompileScope = Scope (Var Value)

programCompile :: ProgramParsed -> Code ProgramCompiled
programCompile program = do
  scope <- Reader.asks (.scope)
  scope0 <- readIORef scope
  scope1 :!: code <- compile scope0 do
    compileBlock program
  writeIORef scope (scopeExtendWith scope1 scope0)
  pure code

compile :: CompileScope -> Compile a -> Code a
compile = flip runReaderT

compileBlock ::
  Parsed Block ->
  Compile (CompileScope :!: Code ())
compileBlock block = do
  scope <- fmap Map do
    for (Map.toList block.scope) compileBinding
  body <- Reader.local (scopeExtendWith scope) do
    for block.body compileStatement
  pure (scope :!: sequenceA_ body)

--  IDEA: Allow shadowing to be a warning.
scopeExtendWith :: CompileScope -> CompileScope -> CompileScope
scopeExtendWith scope scope0 =
  Map.unionWithKey
    (\key _old _new ->
      Prelude.error ("shadowed variable: " <> show key))
    scope0
    scope

compileBinding ::
  (Name, annotation) ->
  Compile (Name, Var Value)
compileBinding (name, _annotation) = do
  var <- lift do varNewLazy throwUninitialized
  pure (name, var)

compileTerm ::
  Parsed Term ->
  Compile (Code ())
compileTerm = \case
  TermName _object name ->
    compileName name
  TermVar _object name ->
    compileName name
  TermValue value -> do
    var <- lift do varOf value
    pure do
      stackPush var
  TermGroup _object body ->
    compileExpression body
  TermBlock _object block -> fmap snd do
    compileBlock block

compileExpression ::
  Parsed Expression ->
  Compile (Code ())
compileExpression expression = do
  terms <- for expression.terms compileTerm
  pure do
    sequenceA_ (Seq.reverse terms)

compileStatement ::
  Parsed Expression ->
  Compile (Code ())
compileStatement = compileExpression

compileName ::
  Name ->
  Compile (Code ())
compileName name = Reader.asks (Map.lookup name) >>= \case
  Lazy.Just var -> pure do
    stackPush var
  Lazy.Nothing ->
    throwError (toException (Error "missing name"))

----------------------------------------------------------------
