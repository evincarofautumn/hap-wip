{-# Language BlockArguments #-}
{-# Language DeriveAnyClass #-}
{-# Language DerivingStrategies #-}
{-# Language DerivingVia #-}
{-# Language DuplicateRecordFields #-}
{-# Language LambdaCase #-}
{-# Language NoFieldSelectors #-}
{-# Language NumDecimals #-}
{-# Language OverloadedLabels #-}
{-# Language OverloadedRecordDot #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
{-# Options_ghc -Wno-unused-imports #-}

module Main where

import Control.Concurrent (forkIO, forkOS)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.Chan qualified as Chan
import Control.Concurrent.STM.TBQueue (TBQueue, flushTBQueue)
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM.TChan qualified as TChan
import Control.Concurrent.STM.TVar (TVar, modifyTVar', readTVar, stateTVar, writeTVar)
import Control.Concurrent.STM.TVar qualified as TVar
import Control.Exception (Exception, SomeException(SomeException), finally, throwIO, try)
import Control.Exception qualified as Exception
import Control.Monad ((<=<), join, void, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Except (MonadError, ExceptT)
import Control.Monad.Except qualified as Error
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Reader qualified as Reader
import Control.Monad.STM (STM, orElse)
import Control.Monad.STM qualified as STM
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (MonadWriter, WriterT)
import Control.Monad.Writer qualified as Writer
import Data.ByteString qualified as ByteString
import Data.Coerce (coerce)
import Data.DList (DList)
import Data.DList qualified as DList
import Data.Either qualified as Lazy (partitionEithers)
import Data.Foldable (for_, toList)
import Data.Function ((&))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
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
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Read qualified as Text.Read
import Data.Traversable (for)
import Data.Void (Void)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Hap qualified
import Optics ((%~))
import Optics qualified
import Prelude hiding (Either(..), Maybe(..), Word, error, id, lines, maybe)
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
import System.Timeout qualified as Timeout

main :: IO ()
main = start defaultSettings

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
    startInput = do
      hapRun world do
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

compileOne :: Source -> Code ProgramCompiled
compileOne source = do
  loaded <- (cellNew . programLoad) source
  tokenized <- (cellNew . fmap programTokenize . cellGet) loaded
  parsed <- (cellNew . (programParse <=< cellGet)) tokenized
  compiled <- (cellNew . (programCompile <=< cellGet)) parsed
  results <- cellGet compiled
  pure results

compileAll :: Sources -> Code (Seq ProgramCompiled)
compileAll sources = do
  loaded <- traverse (cellNew . programLoad) sources
  tokenized <- traverse (cellNew . fmap programTokenize . cellGet) loaded
  parsed <- traverse (cellNew . (programParse <=< cellGet)) tokenized
  compiled <- traverse (cellNew . (programCompile <=< cellGet)) parsed
  results <- traverse cellGet compiled
  pure results

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
    --  Error.liftEither (Lazy.Left exitCode)

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
  --  IDEA: Split 'CacheFull' to save an indirection.
  = CacheFull !(Result a)
  | CacheEmpty
  | CacheFilling

type Result =
  Either SomeException

interpret :: Text -> IO (Maybe Value)
interpret text = do
  world <- worldNew
  let
    timeout_s = 2
    micro n = 1e6 * n
  fmap toStrict do
    Timeout.timeout (micro timeout_s) do
      hapRun world do
        compiled <- compileOne (SourceText text)
        compiled

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
cellNew code0 = do
  allocator <- Reader.asks (.allocator)
  id <- liftIO (idNew allocator)
  code <- newTVarIO code0
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
        Nothing -> do
          consoleOutput "unknown command"
          consoleLoop
      Lazy.Nothing -> do
        world <- Reader.asks (.world)
        result <- liftIO
          (hapRun world (join (compileOne (SourceText line))))
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
pattern Seq list <- (toList -> list)
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
--  Container Wrappers
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

----------------------------------------------------------------
--  MonadIO Wrappers
----------------------------------------------------------------

atomically :: (MonadIO m) => STM a -> m a
atomically = liftIO . STM.atomically

newTBQueueIO :: (MonadIO m) => Natural -> m (TBQueue a)
newTBQueueIO = liftIO . TBQueue.newTBQueueIO

newTChanIO :: (MonadIO m) => m (TChan a)
newTChanIO = liftIO TChan.newTChanIO

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
type ProgramParsed = Block
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

newtype Expression = Expression { terms :: Terms }
  deriving stock (Generic, Show)

type Terms = Seq Term

data Term
  = TermName !Name
  | TermNumber !Number
  | TermText !Text
  | TermGroup !Expression
  | TermBlock !Block
  deriving stock (Generic, Show)

data Value
  = ValueNumber !Number
  | ValueText !Text
  deriving stock (Generic, Show)

data Block = Block !Bindings !Expression
  deriving stock (Generic, Show)

type Bindings = Set Name

newtype Name =
  Name { spelling :: Text }
  deriving stock (Eq, Generic, Ord, Show)

type Number = Integer

data ParseError
  = ParseError
  deriving anyclass (Exception)
  deriving stock (Generic, Show)

programLoad :: Source -> Code ProgramLoaded
programLoad = \case
  SourcePath path ->
    fmap Text.Encoding.decodeUtf8 do
      liftIO (ByteString.readFile path)
  SourceText text ->
    pure text

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

type BindingSites = Union (Map Name (Seq ()))

type Parsed a = (a, BindingSites, Tokens)

programParse :: ProgramTokenized -> Code ProgramParsed
programParse = parseAllTerms

parseAllTerms :: (MonadIO m) => Tokens -> m ProgramParsed
parseAllTerms tokens0
  | null tokens1 =
    pure (Block bindings (Expression (Seq terms)))
  | otherwise =
    throw ParseError
  where
    (terms, sites, tokens1) = parseManyTerms tokens0
    bindings = bindingsFromSites sites

--  IDEA: Handle duplicate bindings and annotations here.
bindingsFromSites :: BindingSites -> Bindings
bindingsFromSites = Map.keysSet . (.union)

parseManyTerms :: Tokens -> Parsed [Term]
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
  m (Parsed Term)
parseOneTerm = \case
  [] -> Error.throwError ParseError
  token : tokens1 -> case token of
    TokenKeyword KeywordVar
      | TokenWord word : tokens2 <- tokens1 -> let
        name = Name word
        in pure
          (
            TermName name,
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
          (TermName (Name word))
          TermNumber
          (parseNumber word)
    TokenText text ->
      pure (TermText text, mempty, tokens1)
    TokenGroupBegin -> case tokens2 of
      TokenGroupEnd : tokens3 ->
        pure
          (
            TermGroup (Expression (Seq terms)),
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
          block = TermBlock
            (Block bindings (Expression (Seq terms)))
      _ ->
        Error.throwError ParseError
      where
        (terms, sites, tokens2) = parseManyTerms tokens1
        bindings = bindingsFromSites sites
    TokenBlockEnd ->
      Error.throwError ParseError

parseNumber :: Text -> Maybe Number
parseNumber word
  | Lazy.Right (number, TextEmpty) <-
      Text.Read.signed Text.Read.decimal word =
    Just number
  | otherwise =
    Nothing

data CompileError
  = CompileError
  deriving anyclass (Exception)
  deriving stock (Generic, Show)

programCompile :: ProgramParsed -> Code ProgramCompiled
programCompile _program = throw CompileError

----------------------------------------------------------------
