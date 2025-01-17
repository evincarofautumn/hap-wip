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
    Either (..),
    Maybe (..),
    Word,
    (.),
    cycle,
    error,
    exp,
    exponent,
    fst,
    head,
    id,
    init,
    last,
    lines,
    maybe,
    significand,
    snd,
    sum,
    tail,
    uncurry,
  )
import Prelude qualified
import Prelude qualified as Lazy
  (
    Either (..),
    Maybe (..),
    fst,
    maybe,
    snd,
    uncurry,
  )

import Control.Applicative (liftA3)
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
import Control.Concurrent.STM.TQueue
  (
    TQueue,
    writeTQueue,
    flushTQueue
  )
import Control.Concurrent.STM.TQueue qualified as TQueue
import Control.Concurrent.STM.TVar
  (
    TVar,
    modifyTVar',
    newTVar,
    readTVar,
    stateTVar,
    writeTVar
  )
import Control.Concurrent.STM.TVar qualified as TVar
import Control.Exception
  (
    Exception (displayException, fromException, toException),
    SomeAsyncException (..),
    SomeException (..),
    finally,
    try,
  )
import Control.Exception qualified as Exception
import Control.Monad ((<=<), (>=>), join, void, when)
import Control.Monad.Except
  (
    MonadError,
    ExceptT,
    throwError,
    tryError,
  )
import Control.Monad.Except qualified as Error
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
import Control.Monad.Reader qualified as Reader
import Control.Monad.STM (STM, orElse)
import Control.Monad.STM qualified as STM
import Control.Monad.State.Strict (MonadState, StateT)
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Writer (MonadWriter, WriterT)
import Control.Monad.Writer qualified as Writer
import Data.Array (Array)
import Data.Array qualified as Array
import Data.ByteString qualified as ByteString
import Data.Char qualified as Char
import Data.Coerce (coerce)
import Data.DList (DList)
import Data.DList qualified as DList
import Data.Either qualified as Lazy (partitionEithers)
import Data.Foldable (fold, for_, sequenceA_, toList, traverse_)
import Data.Foldable qualified as Foldable
import Data.Function ((&), on)
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.Ix (Ix)
import Data.Kind (Constraint, Type)
import Data.List (partition)
import Data.Map.Strict (Map, (!?))
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Lazy (fromMaybe)
import Data.Monoid (Ap (Ap, getAp))
import Data.Monoid qualified as Monoid
import Data.Ratio qualified as Ratio
import Data.Semigroup (Last(Last, getLast))
import Data.Sequence (Seq((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Strict.Classes (toLazy, toStrict)
import Data.Strict.Either (Either (Left, Right))
import Data.Strict.Maybe
  (
    Maybe (Nothing, Just),
    catMaybes,
    maybe
  )
import Data.Strict.Tuple
  (
    type (:!:),
    pattern (:!:),
    fst,
    snd,
    uncurry
  )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.IO qualified as Text.IO
import Data.Text.Read qualified as Text.Read
import Data.Traversable (for)
import Data.Tuple qualified as Tuple
import Data.Void (Void, absurd, vacuous)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import GHC.Generics (Generic)
import GHC.Exts (IsString)
import GHC.Exts qualified
import GHC.Natural (Natural)
import Optics ((%~))
import Optics qualified
import Prettyprinter
  (
    Pretty (pretty),
    (<+>),
    hang,
    hcat,
    hsep,
    sep,
    vcat,
    vsep,
  )
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.Text qualified
import SDL (($=))
import SDL qualified
import System.Console.Haskeline qualified as Haskeline
import System.Console.Haskeline.IO qualified as Haskeline.IO
import System.Environment qualified as Environment
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPrint, hPutStrLn, stderr)
import System.Mem.Weak (Weak)
import System.Mem.Weak qualified as Weak
import System.Timeout qualified as Timeout

main :: IO ()
main = do
  args <- Environment.getArgs
  startIO
    defaultSettings {
      sources = Seq [SourcePath path | path <- args]
    }

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
  world <- worldNew
  compiledPrograms <- codeRun world do
    loaded <- traverse programLoad settings.sources
    compileAll loaded

  let
    startInput = do
      codeRun world do
        for_ compiledPrograms \program -> do
          void program
          flush
      exitCode <- case settings.input of
        InputBatch ->
          pure ExitSuccess
        InputInteractive ->
          consoleRun world
      messagesRequest world.messages (RequestExit exitCode)

    startOutput =
      case settings.output of
        OutputGraphics ->
          graphicsRun world.messages
        OutputText ->
          messagesLoop
            world.messages
            (pure ())
            (\_request -> pure ())

  exitWith =<< Async.withAsync startInput \_input ->
    startOutput

compileOne ::
  ProgramLoaded ->
  Code ProgramCompiled
compileOne loaded = do
  tokenized <- (varNewStrict . pure . programTokenize . programLex) loaded
  parsed <- (varNewStrict . (programParse <=< varGet)) tokenized
  compiled <- (programCompile <=< varGet) parsed
  pure compiled

compileAll ::
  Seq ProgramLoaded ->
  Code (Seq ProgramCompiled)
compileAll loaded = do
  tokenized <- traverse (varNewStrict . pure . programTokenize . programLex) loaded
  parsed <- traverse (varNewStrict . (programParse <=< varGet)) tokenized
  compiled <- traverse (programCompile <=< varGet) parsed
  pure compiled

----------------------------------------------------------------
--  Output Thread
----------------------------------------------------------------

newtype Messages =
  Messages {
    requests :: TBQueue Request
  }
  deriving stock (Generic)

data Request
  = RequestOutput !Text
  | RequestDrawClear
  | RequestDrawColor !(SDL.V4 Word8)
  | RequestDrawLine
    !(SDL.Point SDL.V2 CInt)
    !(SDL.Point SDL.V2 CInt)
  | RequestDrawRect !(Lazy.Maybe (SDL.Rectangle CInt))
  | RequestDrawShow
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
  let timeout_ms = 10
  events <- liftA2 (Lazy.maybe identity (:))
    (SDL.waitEventTimeout timeout_ms)
    SDL.pollEvents
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
    pure ()
  SDL.WindowExposedEvent{} -> do
    pure ()
  _ -> pure ()

graphicsRequest ::
  Graphics ->
  Request ->
  ExceptT ExitCode IO ()
graphicsRequest graphics = \case
  RequestOutput text -> lift do
    Text.IO.putStrLn text
  RequestDrawClear -> lift do
    SDL.clear graphics.renderer
  RequestDrawColor color -> lift do
    SDL.rendererDrawColor graphics.renderer $= color
  RequestDrawLine point1 point2 -> lift do
    SDL.drawLine graphics.renderer point1 point2
  RequestDrawRect rects -> lift do
    SDL.fillRect graphics.renderer rects
  RequestDrawShow -> lift do
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
    (either identity (const ExitSuccess))
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

throw ::
  (MonadError SomeException m) =>
  Doc ->
  m bottom
throw = Error.throwError . toException . Error

uninitialized ::
  (MonadError SomeException m) =>
  m bottom
uninitialized = throw "uninitialized"

unsolved ::
  (MonadError SomeException m) =>
  m bottom
unsolved = throw "unsolved"

cycle ::
  (MonadError SomeException m) =>
  m bottom
cycle = throw "cycle"

useRun :: Use a b -> a -> Code b
useRun = \case
  UseVar varB -> \_a -> varGet varB
  UseBind needA -> \a -> needA a

putRun :: Put -> Code ()
putRun (Put code var) = varPut var =<< code

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
    queue :: !(TQueue Put),
    messages :: !Messages
  }
  deriving stock (Generic)

stackPeek :: Code (Var Value)
stackPeek = do
  stack <- Reader.asks (.stack)
  readIORef stack >>= \case
    [] -> throw "underflow"
    top : _down -> pure top

stackPop :: Code (Var Value)
stackPop = do
  stack <- Reader.asks (.stack)
  readIORef stack >>= \case
    [] -> throw "underflow"
    top : down -> do
      writeIORef stack down
      pure top

stackPush :: Var Value -> Code ()
stackPush value = do
  stack <- Reader.asks (.stack)
  modifyIORef' stack (value :)

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

limit :: Int -> [Doc] -> [Doc]
limit n xs = case splitAt n xs of
  ([], []) -> []
  (before, []) -> before
  (before, _after) -> before <> ["..."]

commas :: [Doc] -> [Doc]
commas = Pretty.punctuate ","

instance Debug Int where
  debug = pretty

instance Debug Text where
  debug = pretty

instance Debug () where
  debug = pretty

instance (Debug a, Debug b) => Debug (a, b) where
  debug (a, b) =
    (sep . commas) [debug a, debug b]

instance (Debug a, Debug b, Debug c) => Debug (a, b, c) where
  debug (a, b, c) =
    (sep . commas) [debug a, debug b, debug c]

instance (Debug a) => Debug (Identity a) where
  debug = debug . runIdentity

instance (Debug a) => Debug (Maybe a) where
  debug = debug . each

instance (Debug a) => Debug [a] where
  debug = sep . commas . fmap debug

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
  result <- codeRun world code
  (liftIO . print . debug) result
  pure result

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
          throwIO error
        StoreEmpty -> throwIO (Error "result empty (not begun)")
        StoreFilling -> throwIO (Error "result empty (not done)")
    Left error -> do
      throwIO error
    Right _queue -> throwIO (Error "ended with work left")

  where

    loop :: [Put] -> IO (Result [Put])
    loop (Put code var : queue0) = do
      answers <- try (code.run world)
      case answers of
        Lazy.Left error
          | Lazy.Just SomeAsyncException{} <-
              fromException error ->
            throwIO error
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
  messages <- messagesNew
  pure World { next, heap, scope, stack, queue, messages }

heapNew :: IO (TVar Heap)
heapNew = newTVarIO idMapEmpty

flush :: Code ()
flush = do
  queue <- Reader.asks (.queue)
  actions <- atomically do
    flushTQueue queue
  for_ actions putRun

varForce :: Var a -> Code ()
varForce = void . varGet

varGet :: Var a -> Code a
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
      cycle

varPut :: Var a -> a -> Code ()
varPut var = varPutResult var . Right

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
  varNewFrom world StratLazy uninitialized

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

send :: Request -> Code ()
send request = do
  messages <- Reader.asks (.messages)
  messagesRequest messages request

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
  inputState <- Haskeline.IO.initializeInput
    Haskeline.defaultSettings
  let console = Console { inputState, world }
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
          compiled <- compileOne line
          compiled
          flush
        consoleOutput =<< case results of
          Right () ->
            fmap
              (prettyText . debug)
              (stackSnap =<< readIORef world.stack)
          Left error ->
            (pure . Text . displayException) error
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

{-# Inline identity #-}
identity :: a -> a
identity = Prelude.id

----------------------------------------------------------------
--  Extensions
----------------------------------------------------------------

liftA4 ::
  (Applicative f) =>
  (a -> b -> c -> d -> e) ->
  f a ->
  f b ->
  f c ->
  f d ->
  f e
liftA4 f fa fb fc fd =
  f <$> fa <*> fb <*> fc <*> fd

sequence0 ::
  (Applicative f, Foldable t) =>
  t (f a) ->
  f (Lazy.Maybe a)
sequence0 =
  fmap Monoid.getLast .
  getAp .
  foldMap (Ap . fmap (Monoid.Last . Lazy.Just))

----------------------------------------------------------------
--  Character Set
----------------------------------------------------------------

pattern CharLineFeed :: Char
pattern CharLineFeed = '\x000A'

pattern CharExclamationMark :: Char
pattern CharExclamationMark = '\x0021'

pattern CharQuotationMark :: Char
pattern CharQuotationMark = '\x0022'

pattern CharNumberSign :: Char
pattern CharNumberSign = '\x0023'

pattern CharPercentSign :: Char
pattern CharPercentSign = '\x0025'

pattern CharAmpersand :: Char
pattern CharAmpersand = '\x0026'

pattern CharApostrophe :: Char
pattern CharApostrophe = '\x0027'

pattern CharLeftParenthesis :: Char
pattern CharLeftParenthesis = '\x0028'

pattern CharRightParenthesis :: Char
pattern CharRightParenthesis = '\x0029'

pattern CharAsterisk :: Char
pattern CharAsterisk = '\x002A'

pattern CharPlusSign :: Char
pattern CharPlusSign = '\x002B'

pattern CharComma :: Char
pattern CharComma = '\x002C'

pattern CharHyphenMinus :: Char
pattern CharHyphenMinus = '\x002D'

pattern CharFullStop :: Char
pattern CharFullStop = '\x002E'

pattern CharSolidus :: Char
pattern CharSolidus = '\x002F'

pattern CharColon :: Char
pattern CharColon = '\x003A'

pattern CharSemicolon :: Char
pattern CharSemicolon = '\x003B'

pattern CharLessThanSign :: Char
pattern CharLessThanSign = '\x003C'

pattern CharEqualsSign :: Char
pattern CharEqualsSign = '\x003D'

pattern CharGreaterThanSign :: Char
pattern CharGreaterThanSign = '\x003E'

pattern CharQuestionMark :: Char
pattern CharQuestionMark = '\x003F'

pattern CharCommercialAt :: Char
pattern CharCommercialAt = '\x0040'

pattern CharReverseSolidus :: Char
pattern CharReverseSolidus = '\x005C'

pattern CharCircumflexAccent :: Char
pattern CharCircumflexAccent = '\x005E'

pattern CharLowLine :: Char
pattern CharLowLine = '\x005F'

pattern CharLeftCurlyBracket :: Char
pattern CharLeftCurlyBracket = '\x007B'

pattern CharVerticalLine :: Char
pattern CharVerticalLine = '\x007C'

pattern CharRightCurlyBracket :: Char
pattern CharRightCurlyBracket = '\x007D'

pattern CharTilde :: Char
pattern CharTilde = '\x007E'

charIsTextEnd :: Char -> Bool
charIsTextEnd = \case
  CharLineFeed -> True
  CharQuotationMark -> True
  _ -> False

charIsWord :: Char -> Bool
charIsWord = \case
  char
    | Char.isLetter char -> True
  _ -> False

charIsJoiner :: Char -> Bool
charIsJoiner = \case
  CharApostrophe -> True
  CharHyphenMinus -> True
  CharLowLine -> True
  _ -> False

charIsSymbol :: Char -> Bool
charIsSymbol = \case
  CharExclamationMark -> True
  CharNumberSign -> True
  CharPercentSign -> True
  CharAmpersand -> True
  CharAsterisk -> True
  CharPlusSign -> True
  CharHyphenMinus -> True
  CharFullStop -> True
  CharSolidus -> True
  CharLessThanSign -> True
  CharEqualsSign -> True
  CharGreaterThanSign -> True
  CharQuestionMark -> True
  CharCommercialAt -> True
  --  CharLowLine -> True
  CharReverseSolidus -> True
  _ -> False

--  Reserved:
--
--  * U+0024 dollar sign
--  * U+005E circumflex accent
--  * U+0060 grave accent
--  * U+007E tilde

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
  deriving newtype (Debug)

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

throwIO :: (Exception e, MonadIO m) => e -> m z
throwIO = liftIO . Exception.throwIO

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
type ProgramLexed = Lexels
type ProgramTokenized = Tokens
type ProgramParsed = Parsed Block
type ProgramCompiled = Code ()

type Lexels = [Lexel]

type Lexel = Maybe Token

type Tokens = [Token]

data Token
  = TokenName !Text
  | TokenSymbol !Text
  | TokenJoiner !Text
  | TokenIntegral !(Maybe Sign) !Text
  | TokenDecimal !(Maybe Sign) !Text !Text
  | TokenRational !(Maybe Sign) !Text !Text
  | TokenScientific !(Maybe Sign) !Text !Text !(Maybe Sign) !Text
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
    TokenName name ->
      pretty name
    TokenSymbol symbol ->
      pretty symbol
    TokenJoiner joiner ->
      pretty joiner
    TokenIntegral signs natural ->
      foldMap debug signs <> pretty natural
    TokenDecimal signs whole fraction ->
      hcat [
        foldMap debug signs,
        pretty whole,
        pretty CharFullStop,
        pretty fraction
      ]
    TokenRational signs numerator denominator ->
      hcat [
        foldMap debug signs,
        pretty numerator,
        pretty CharSolidus,
        pretty denominator
      ]
    TokenScientific signs whole fraction exponentSigns exponent ->
      hcat [
        foldMap debug signs,
        pretty whole,
        pretty CharFullStop,
        pretty fraction,
        "e",
        foldMap debug exponentSigns,
        pretty exponent
      ]
    TokenKeyword keyword ->
      hcat [pretty CharColon, pretty keyword]
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

data Sign
  = SignNeg
  | SignPos
  deriving stock (Generic, Show)

instance Debug Sign where
  debug SignNeg = "-"
  debug SignPos = "+"

--  data Base
--    = BaseDecimal
--    deriving stock (Generic, Show)

--  data Figure =
--    Figure {
--      sign :: !Sign,
--      base :: !Base,
--      significand :: !Integer,
--      shift :: !(Maybe Nat),
--      exponent :: !(Maybe Int)
--    }
--    deriving stock (Generic, Show)

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
        hcat [pretty CharColon, pretty KeywordVar],
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
  | ValueNatural !Natural
  | ValueInteger !Integer
  | ValueRational !Rational
  | ValueText !Text
  deriving stock (Generic, Show)

instance Debug Value where
  debug = \case
    ValueNone -> "none"
    ValueNatural natural -> pretty natural
    ValueInteger integer -> pretty integer
    ValueRational rational ->
      hcat [
        pretty (Ratio.numerator rational),
        "/",
        pretty (Ratio.denominator rational)
      ]
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

programTokenize :: ProgramLexed -> ProgramTokenized
programTokenize =
  catMaybes .
  tokenizeSuffixes .
  tokenizeSigns .
  tokenizeFractions .
  tokenizeScientifics .
  tokenizeDecimals .
  tokenizeJoiners .
  (Nothing :) .
  (<> [Nothing])

tokenizeJoiners :: Lexels -> Lexels

--  Digits separated by a joiner are joined as digits.
tokenizeJoiners
  (
    Just (TokenIntegral Nothing digits1) :
    Just (TokenJoiner "_") :
    Just (TokenIntegral Nothing digits2) :
    lexels1
  )
  = tokenizeJoiners
  (
    Just (TokenIntegral Nothing (digits1 <> digits2)) :
    lexels1
  )

--  A name and a joiner are joined as a name.
tokenizeJoiners
  (
    Just (TokenName name) :
    Just (TokenJoiner joiner) :
    lexels1
  )
  | joiner `elem` ["-", "'"]
  = tokenizeJoiners
  (
    Just (TokenName (name <> joiner)) :
    lexels1
  )

--  A name and a name are joined as a name.
tokenizeJoiners
  (
    Just (TokenName name1) :
    Just (TokenName name2) :
    lexels1
  )
  = tokenizeJoiners
  (
    Just (TokenName (name1 <> name2)) :
    lexels1
  )

tokenizeJoiners (lexel : lexels1) =
  lexel : tokenizeJoiners lexels1

tokenizeJoiners [] = []

tokenizeDecimals :: Lexels -> Lexels

--  Digits separated by a decimal point
--  are joined into a fractional number.
tokenizeDecimals
  (
    Just (TokenIntegral Nothing whole) :
    Just (TokenSymbol ".") :
    Just (TokenIntegral Nothing fraction) :
    lexels1
  )
  =
  (
    Just (TokenDecimal Nothing whole fraction) :
    tokenizeDecimals lexels1
  )

--  Digits preceded by a decimal point
--  are joined into a fractional number.
tokenizeDecimals
  (
    Just (TokenSymbol ".") :
    Just (TokenIntegral Nothing fraction) :
    lexels1
  ) =
    Just (TokenDecimal Nothing "" fraction) :
    tokenizeDecimals lexels1

--  Digits followed by a decimal point
--  are joined into a fractional number.
tokenizeDecimals
  (
    Just (TokenIntegral Nothing whole) :
    Just (TokenSymbol ".") :
    lexels1
  ) =
    Just (TokenDecimal Nothing whole "") :
    tokenizeDecimals lexels1

tokenizeDecimals (lexel : lexels1) =
  lexel : tokenizeDecimals lexels1

tokenizeDecimals [] = []

tokenizeScientifics :: Lexels -> Lexels

--  An integral or fractional number
--  and an exponent with an optional sign
--  are joined into a scientific number.
tokenizeScientifics
  (Just token : Just (TokenName "e") : lexels1)
  | Just (signs :!: (whole :!: fraction)) <- case token of
      TokenIntegral signs whole ->
        Just (signs :!: (whole :!: ""))
      TokenDecimal signs whole fraction ->
        Just (signs :!: (whole :!: fraction))
      _ -> Nothing,
    Just ((exponentSigns :!: exponent) :!: lexels2) <-
      case lexels1 of
        Just (TokenIntegral Nothing exponent) :
          lexels2 ->
          Just ((Nothing :!: exponent) :!: lexels2)
        Just (TokenSymbol "+") :
          Just (TokenIntegral Nothing exponent) :
          lexels2 ->
          Just ((Just SignPos :!: exponent) :!: lexels2)
        Just (TokenJoiner "-") :
          Just (TokenIntegral Nothing exponent) :
          lexels2 ->
          Just ((Just SignNeg :!: exponent) :!: lexels2)
        _ -> Nothing

  =
    Just
      (TokenScientific
        signs
        whole
        fraction
        exponentSigns
        exponent) :
    tokenizeScientifics lexels2

tokenizeScientifics (lexel : lexels1) =
  lexel : tokenizeScientifics lexels1

tokenizeScientifics [] = []

--  Whole numbers separated by a slash
--  are joined into a fraction.
tokenizeFractions :: Lexels -> Lexels

tokenizeFractions
  (
    Just (TokenIntegral Nothing whole1) :
    Just (TokenSymbol "/") :
    Just (TokenIntegral Nothing whole2) :
    lexels1
  )
  =
  Just (TokenRational Nothing whole1 whole2) :
  tokenizeFractions lexels1

tokenizeFractions (lexel : lexels1) =
  lexel : tokenizeFractions lexels1

tokenizeFractions [] = []

tokenizeSigns :: Lexels -> Lexels

--  A sign and a number are joined into a signed number.
tokenizeSigns (Just token1 : Just token2 : lexels1)
  | signs@Just{} <- case token1 of
      TokenJoiner "-" -> Just SignNeg
      TokenSymbol "+" -> Just SignPos
      _ -> Nothing,
    lexels@Just{} <- case token2 of
      TokenIntegral Nothing whole ->
        Just (TokenIntegral signs whole)
      TokenDecimal Nothing whole fraction ->
        Just (TokenDecimal signs whole fraction)
      TokenRational Nothing numerator denominator ->
        Just (TokenRational signs numerator denominator)
      TokenScientific
        Nothing
        whole
        fraction
        exponentSigns
        exponent ->
        Just
          (TokenScientific
            signs
            whole
            fraction
            exponentSigns
            exponent)
      _ -> Nothing
  =
    lexels : tokenizeSigns lexels1

tokenizeSigns (lexel : lexels1) =
  lexel : tokenizeSigns lexels1

tokenizeSigns [] = []

tokenizeSuffixes :: Lexels -> Lexels

--  Names followed by digits are joined as a name
--  as long as they were not part of a number, like @e3@.
tokenizeSuffixes
  (
    Just (TokenName name) :
    Just (TokenIntegral Nothing digits) :
    lexels1
  )
  = tokenizeSuffixes
  (
    Just (TokenName (name <> digits)) :
    lexels1
  )

tokenizeSuffixes (lexel : lexels1) =
  lexel : tokenizeSuffixes lexels1

tokenizeSuffixes [] = []

programLex :: ProgramLoaded -> ProgramLexed
programLex = \case
  TextEmpty -> []
  TextCons char chars1
    | CharQuotationMark <- char -> let
      (text, chars2) = Text.break charIsTextEnd chars1
      in Just (TokenText text) : programLex (Text.drop 1 chars2)
    | CharColon <- char -> case lexSymbol chars1 of
      Just (keyword :!: chars2) ->
        Just (TokenKeyword keyword) : programLex chars2
      Nothing -> case lexWord chars1 of
        Just (keyword :!: chars2) ->
          Just (TokenKeyword keyword) : programLex chars2
        Nothing ->
          Just (TokenKeyword "") : programLex chars1
    | Just token <- matchPunctuation char ->
      Just token : programLex chars1
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
    | Just (number :!: chars1) <- lexNumber chars0 ->
      Just (TokenIntegral Nothing number) : programLex chars1
    | Just (joiner :!: chars1) <- lexJoiner chars0 ->
      Just (TokenJoiner joiner) : programLex chars1
    | Just (symbol :!: chars1) <- lexSymbol chars0 ->
      Just (TokenSymbol symbol) : programLex chars1
    | Just (name :!: chars1) <- lexWord chars0 ->
      Just (TokenName name) : programLex chars1
    | Just (_ :!: chars1) <- lexSome Char.isSpace chars0 ->
      Nothing : programLex chars1
    | otherwise ->
      Prelude.error
        ("unhandled input: " <> show (Text.unpack chars0))

lexNumber :: Text -> Maybe (Text :!: Text)
lexNumber = lexSome Char.isDigit

lexWord :: Text -> Maybe (Text :!: Text)
lexWord = lexSome charIsWord

lexJoiner :: Text -> Maybe (Text :!: Text)
lexJoiner = lexSome charIsJoiner

lexSymbol :: Text -> Maybe (Text :!: Text)
lexSymbol = lexSome charIsSymbol

lexSome :: (Char -> Bool) -> Text -> Maybe (Text :!: Text)
lexSome predicate chars0 = let
  (result, chars1) = Text.span predicate chars0
  in if Text.null result
    then Nothing
    else Just (result :!: chars1)

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
    (throw . vsep) [
      "unknown statement:",
      (hang 4 . vsep . limit 8) [
        hsep ["*", debug token]
        | token <- tokens1
      ]
    ]
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
  [] -> throw "missing term"
  token : tokens1 -> case token of
    TokenName name ->
      pure (term, mempty, tokens1)
      where
        term = TermName () (Name name)
    TokenSymbol symbol ->
      throw ("unknown symbol:" <+> debug symbol)
    TokenJoiner joiner ->
      throw ("unknown joiner:" <+> debug joiner)
    TokenKeyword KeywordVar
      | TokenName name : tokens2 <- tokens1 -> let
        in pure
          (
            TermVar () (Name name),
            (Union . Map) [(Name name, Seq [()])],
            tokens2
          )
      | otherwise ->
        throw "missing name"
    TokenKeyword keyword ->
      throw ("unknown keyword:" <+> debug keyword)
    TokenIntegral signs digits -> do
      magnitude <- parseDecimal digits
      let
        value = case signs of
          Just SignNeg ->
            ValueInteger (negate (fromIntegral magnitude))
          Just SignPos ->
            ValueInteger (fromIntegral magnitude)
          Nothing ->
            ValueNatural magnitude
        term = TermValue value
      pure (term, mempty, tokens1)
    TokenDecimal signs wholeDigits fractionDigits -> do
      whole <- parseDecimal0 wholeDigits
      fraction <- parseDecimal0 fractionDigits
      let
        shift = Text.length fractionDigits
        magnitude =
          (whole Ratio.% 1)
            + (fraction Ratio.% 10 ^ shift)
        value = case signs of
          Just SignNeg ->
            ValueRational (negate magnitude)
          Just SignPos ->
            ValueRational magnitude
          Nothing ->
            ValueRational magnitude
        term = TermValue value
      pure (term, mempty, tokens1)
    TokenRational signs numeratorDigits denominatorDigits -> do
      numerator <- parseDecimal numeratorDigits
      denominator <- parseDecimal denominatorDigits
      when (denominator == 0) do
        (throw . sep) ["undefined rational", debug token]
      let
        magnitude = numerator Ratio.% denominator
        value = case signs of
          Just SignNeg ->
            ValueRational (negate magnitude)
          Just SignPos ->
            ValueRational magnitude
          Nothing ->
            ValueRational magnitude
        term = TermValue value
      pure (term, mempty, tokens1)
    TokenScientific
      signs
      wholeDigits
      fractionDigits
      exponentSigns
      exponentDigits -> do
      whole <- parseDecimal0 wholeDigits
      fraction <- parseDecimal0 fractionDigits
      exponent <- parseDecimal0 @Int exponentDigits
      let
        shift = Text.length fractionDigits
        significand =
          (whole Ratio.% 1)
            + fraction Ratio.% 10 ^ shift
        scale = 10 ^ exponent
        magnitude = case exponentSigns of
          Just SignNeg -> significand / scale
          Just SignPos -> significand * scale
          Nothing -> significand * scale
        value = ValueRational case signs of
          Just SignNeg -> negate magnitude
          Just SignPos -> magnitude
          Nothing -> magnitude
        term = TermValue value
      pure (term, mempty, tokens1)
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
        throw "missing group end"
      where
        (terms, sites, tokens2) = parseManyTerms tokens1
    TokenGroupEnd ->
      throw "unmatched group end"
    TokenBlockBegin -> case tokens2 of
      TokenBlockEnd : tokens3 ->
        pure (block, mempty, tokens3)
        where
          block = TermBlock ()
            (Block scope (Seq statements))
      _ ->
        throw "missing block end"
      where
        ~(statements, sites, tokens2) =
          parseManyStatements tokens1
        scope = scopeFromSites sites
    TokenBlockEnd ->
      throw "unmatched block end"
    TokenExpressionSeparator ->
      throw "misplaced expression separator"
    TokenStatementSeparator ->
      throw "misplaced statement separator"

parseDecimal0 ::
  (Integral i, MonadError SomeException m) =>
  Text ->
  m i
parseDecimal0 TextEmpty = pure 0
parseDecimal0 digits = parseDecimal digits

parseDecimal ::
  (Integral i, MonadError SomeException m) =>
  Text ->
  m i
parseDecimal digits =
  case Text.Read.decimal digits of
    Lazy.Right (value, TextEmpty) ->
      pure value
    _ ->
      throw ("unknown number:" <+> pretty digits)

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
  var <- lift do varNewLazy uninitialized
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
  --  do
  --    _scope :!: body <- do
  --      compileBlock block
  --    var <- lift do
  --      varNewStrict do
  --        body
  --        pure ValueNone
  --    pure do
  --      stackPush var

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
compileName name = Reader.asks (!? name) >>= \case
  Lazy.Just var -> pure do
    stackPush var
  Lazy.Nothing -> case builtinIds !? name of
    Lazy.Just builtin ->
      compileBuiltin builtin
    Lazy.Nothing ->
      throw ("missing name:" <+> debug name)

compileBuiltin ::
  Builtin ->
  Compile (Code ())
compileBuiltin = \case

  BuiltinStackCopy -> pure do
    stackPush =<< stackPeek

  BuiltinStackDrop -> pure do
    void stackPop

  BuiltinStackSwap -> pure do
    former <- stackPop
    latter <- stackPop
    stackPush former
    stackPush latter

  BuiltinVarGet -> pure do
    var <- stackPeek
    varForce var

  BuiltinVarPutIn -> pure do
    var <- stackPop
    val <- stackPop
    varPutLazy var (varGet val)

  BuiltinVarPostTo -> throw "TODO: post-to"

  BuiltinNumAdd ->
    compileBuiltinBinary StratStrict builtinNumAdd
  BuiltinNumSubFrom -> do
    compileBuiltinBinary StratStrict builtinNumSubFrom
  BuiltinNumMul -> do
    compileBuiltinBinary StratStrict builtinNumMul
  BuiltinNumDiv -> do
    compileBuiltinBinary StratStrict builtinNumDiv
  BuiltinNumModOf -> do
    compileBuiltinBinary StratStrict builtinNumModOf

  BuiltinDrawClear -> pure do
    send RequestDrawClear

  BuiltinDrawColor -> pure do
    r <- stackPop
    g <- stackPop
    b <- stackPop
    join do
      liftA3 builtinDrawColor
        (varGet r)
        (varGet g)
        (varGet b)

  BuiltinDrawLine -> pure do
    x1 <- stackPop
    y1 <- stackPop
    x2 <- stackPop
    y2 <- stackPop
    join do
      liftA4 builtinDrawLine
        (varGet x1)
        (varGet y1)
        (varGet x2)
        (varGet y2)

  BuiltinDrawRect -> pure do
    x <- stackPop
    y <- stackPop
    w <- stackPop
    h <- stackPop
    join do
      liftA4 builtinDrawRect
        (varGet x)
        (varGet y)
        (varGet w)
        (varGet h)

  BuiltinDrawShow -> pure do
    send RequestDrawShow

compileBuiltinBinary ::
  Strat ->
  (Value -> Value -> Code Value) ->
  Compile (Code ())
compileBuiltinBinary strat builtin = do
  output <- lift do
    varNew strat uninitialized
  pure do
    input1 <- stackPop
    input2 <- stackPop
    stackPush output
    varPutLazy output do
      join do
        liftA2 builtin (varGet input1) (varGet input2)
    case strat of
      StratStrict -> varForce output
      StratLazy -> pure ()

----------------------------------------------------------------
--  Builtins
----------------------------------------------------------------

data Builtin

  = BuiltinStackCopy
  | BuiltinStackDrop
  | BuiltinStackSwap

  | BuiltinVarGet
  | BuiltinVarPutIn
  | BuiltinVarPostTo

  | BuiltinNumAdd
  | BuiltinNumSubFrom
  | BuiltinNumMul
  | BuiltinNumDiv
  | BuiltinNumModOf

  | BuiltinDrawColor
  | BuiltinDrawClear
  | BuiltinDrawLine
  | BuiltinDrawRect
  | BuiltinDrawShow

  deriving stock (Bounded, Enum, Eq, Generic, Ix, Ord, Show)

builtins :: Array Builtin Name
builtins = Array.array (minBound, maxBound)
  [
    (BuiltinStackCopy,   Name "copy"    ),
    (BuiltinStackDrop,   Name "drop"    ),
    (BuiltinStackSwap,   Name "swap"    ),
    (BuiltinVarGet,      Name "get"     ),
    (BuiltinVarPutIn,    Name "put-in"  ),
    (BuiltinVarPostTo,   Name "post-to" ),
    (BuiltinNumAdd,      Name "add"     ),
    (BuiltinNumSubFrom,  Name "sub-from"),
    (BuiltinNumMul,      Name "mul"     ),
    (BuiltinNumDiv,      Name "div"     ),
    (BuiltinNumModOf,    Name "mod-of"  ),
    (BuiltinDrawColor,   Name "color"   ),
    (BuiltinDrawClear,   Name "clear"   ),
    (BuiltinDrawLine,    Name "line"    ),
    (BuiltinDrawRect,    Name "rect"    ),
    (BuiltinDrawShow,    Name "show"    )
  ]

builtinIds :: Scope Builtin
builtinIds = (Map . fmap Tuple.swap . Array.assocs) builtins

builtinName :: Builtin -> Name
builtinName = (builtins Array.!)

builtinNumAdd :: Value -> Value -> Code Value
builtinNumAdd =
  builtinNumBinary (builtinName BuiltinNumAdd) (+)

builtinNumSubFrom :: Value -> Value -> Code Value
builtinNumSubFrom =
  builtinNumBinary (builtinName BuiltinNumSubFrom) (-)

builtinNumMul :: Value -> Value -> Code Value
builtinNumMul =
  builtinNumBinary (builtinName BuiltinNumMul) (*)

builtinNumDiv :: Value -> Value -> Code Value
builtinNumDiv =
  builtinFractionalBinary (builtinName BuiltinNumDiv) (/)

builtinNumModOf :: Value -> Value -> Code Value
builtinNumModOf =
  builtinIntegralBinary (builtinName BuiltinNumModOf) mod

builtinIntegralBinary ::
  Name ->
  (forall a. (Integral a) => a -> a -> a) ->
  Value -> Value -> Code Value
builtinIntegralBinary name f = curry \case
  (ValueNatural n1, ValueNatural n2) ->
    pure (ValueNatural (f n1 n2))
  (ValueInteger i1, ValueInteger i2) ->
    pure (ValueInteger (f i1 i2))
  (value1, value2) ->
    (throw . sep) [
      "can't apply",
      debug name,
      "to",
      debug value1,
      "and",
      debug value2
    ]

builtinFractionalBinary ::
  Name ->
  (forall a. (Fractional a) => a -> a -> a) ->
  Value -> Value -> Code Value
builtinFractionalBinary name f = curry \case
  (r1@ValueRational{}, ValueRational 0) ->
    (throw . sep) ["can't divide", debug r1, "by zero"]
  (ValueRational r1, ValueRational r2) ->
    pure (ValueRational (f r1 r2))
  (value1, value2) ->
    (throw . sep) [
      "can't apply",
      debug name,
      "to",
      debug value1,
      "and",
      debug value2
    ]

builtinNumBinary ::
  Name ->
  (forall a. (Num a) => a -> a -> a) ->
  Value -> Value -> Code Value
builtinNumBinary name f = curry \case
  (ValueNatural n1, ValueNatural n2) ->
    pure (ValueNatural (f n1 n2))
  (ValueInteger i1, ValueInteger i2) ->
    pure (ValueInteger (f i1 i2))
  (ValueRational r1, ValueRational r2) ->
    pure (ValueRational (f r1 r2))
  (value1, value2) ->
    (throw . sep) [
      "can't apply",
      debug name,
      "to",
      debug value1,
      "and",
      debug value2
    ]

builtinDrawColor ::
  Value ->
  Value ->
  Value ->
  Code ()
builtinDrawColor
  (ValueNatural r)
  (ValueNatural g)
  (ValueNatural b)
  | r <= 255, g <= 255, b <= 255 =
    send
      (RequestDrawColor
        (SDL.V4
          (fromIntegral r)
          (fromIntegral g)
          (fromIntegral b)
          255))

builtinDrawColor value1 value2 value3 =
  (throw . sep) (
    [
      "can't apply",
      debug (builtinName BuiltinDrawColor),
      "to"
    ] <>
    commas [
      debug value1,
      debug value2,
      "and" <+> debug value3
    ]
  )

builtinDrawLine ::
  Value ->
  Value ->
  Value ->
  Value ->
  Code ()
builtinDrawLine
  (ValueNatural x1)
  (ValueNatural y1)
  (ValueNatural x2)
  (ValueNatural y2)
  = send
    (RequestDrawLine
      (SDL.P
        (SDL.V2
          (fromIntegral x1)
          (fromIntegral y1)))
      (SDL.P
        (SDL.V2
          (fromIntegral x2)
          (fromIntegral y2))))

builtinDrawLine value1 value2 value3 value4 =
  (throw . sep) (
    [
      "can't apply",
      debug (builtinName BuiltinDrawLine),
      "to"
    ] <>
    commas [
      debug value1,
      debug value2,
      debug value3,
      "and" <+> debug value4
    ]
  )

builtinDrawRect ::
  Value ->
  Value ->
  Value ->
  Value ->
  Code ()
builtinDrawRect
  (ValueNatural x)
  (ValueNatural y)
  (ValueNatural w)
  (ValueNatural h)
  = send (RequestDrawRect (Lazy.Just rect))
  where
    !rect = SDL.Rectangle
      (SDL.P
          (SDL.V2
            (fromIntegral x)
            (fromIntegral y)))
      (SDL.V2
        (fromIntegral w)
        (fromIntegral h))

builtinDrawRect value1 value2 value3 value4 =
  (throw . sep) (
    [
      "can't apply",
      debug (builtinName BuiltinDrawRect),
      "to"
    ] <>
    commas [
      debug value1,
      debug value2,
      debug value3,
      "and" <+> debug value4
    ]
  )
