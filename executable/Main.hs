{-# LANGUAGE FlexibleInstances, PostfixOperators #-}

------------------------------------------------------------------------------
-- Main Kibro executable

module Main where

import System.Posix.Signals      (installHandler, sigPIPE, Handler(Ignore))
import Prelude hiding (catch)
import Control.Applicative
import Control.Arrow hiding ((<+>))
import Control.Concurrent
import Control.Exception
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Data.Char
import Data.ConfigFile (CPError,ConfigParser)
import qualified Data.ConfigFile as C
import Data.List
import Data.Maybe
import System
import System.IO
import qualified System.IO.Strict as SIO
import System.FilePath
import System.Directory
import System.Posix.Files
import System.Process
import Text.Printf
import Text.Regex

------------------------------------------------------------------------------
-- Main start-up functions

-- | Wrapper for start-up in main executable
main :: IO ()
main = do
  installHandler sigPIPE Ignore Nothing
  getArgs >>= tryCommand

-- | Tries the given command
tryCommand :: [String] -> IO ()
tryCommand [] = do name <- getProgName
                   error $ "no command given (try `" ++ name ++ " help')"
tryCommand (command:args) =
    case find ((==command) . cmdName) commands of
      Just cmd -> startCommand cmd args
      Nothing  -> error $ printf "No such command `%s'" command

-- | Starts the Kibro program's first command
startCommand :: Cmd -> [String] -> IO ()
startCommand cmd args = do
  pwd <- getCurrentDirectory
  name <- getProgName
  let kibroSt = KibroSt { kibProject = Nothing
                        , kibProjDir = error "project directory not defined"
                        , kibCmd     = cmd
                        , kibArgs    = args 
                        , kibAppName = name }
  hSetBuffering stdout NoBuffering
  status <- evalStateT (runErrorT (cmdAct cmd args)) kibroSt
  either error (const $ return ()) status

------------------------------------------------------------------------------
-- Kibro commands

-- | Command list
commands :: [Cmd]
commands = [startCmd,stopCmd,buildCmd,refreshCmd
           ,restartCmd,configureCmd,newCmd,helpCmd]

----------------------------------------
-- `configure' command
configureCmd :: Cmd
configureCmd = Cmd { cmdName = "configure"
                   , cmdDesc = "Use this to rewrite config files (lighttpd.conf, ..)"
                   , cmdFlags = []
                   , cmdAct  = action } where
    action :: [String] -> Command ()
    action _ = do initProject
                  configureProject

configureProject :: Command ()
configureProject = do
  writeLightyConf 

----------------------------------------
-- `refresh' command
refreshCmd :: Cmd
refreshCmd = Cmd { cmdName = "refresh"
                 , cmdDesc = "Rebuild source, if success, restart FastCGI/Lighttpd"
                 , cmdFlags = []
                 , cmdAct  = action } where
    action :: [String] -> Command ()
    action _ = do initProject
                  cmdAct buildCmd []
                  stopFastCGI
                  spawnFCGI

----------------------------------------
-- `restart' command
restartCmd :: Cmd
restartCmd = Cmd { cmdName = "restart"
                 , cmdDesc = "Stop and start Lighttpd and/or FastCGI server"
                 , cmdFlags = []
                 , cmdAct  = action } where
    action :: [String] -> Command ()
    action _ = do initProject
                  cmdAct stopCmd []
                  cmdAct startCmd []

----------------------------------------
-- `stop' command

stopCmd :: Cmd
stopCmd = Cmd { cmdName = "stop"
               , cmdDesc = "Stop Lighttpd and/or FastCGI server"
               , cmdFlags = []
               , cmdAct  = action } where
    action :: [String] -> Command ()
    action _ = do
      initProject
      stopProject

stopProject :: Command ()
stopProject = do stopFastCGI
                 stopLighttpd

stopLighttpd = onlyIfLighttpd $ stopDaemon "lighttpd" "Stopping Lighttpd ... "
stopFastCGI = stopDaemon "fastcgi"  "Stopping FastCGI ... "

stopDaemon :: String -> String -> Command ()
stopDaemon name caption = do
  dirs <- projectDirs
  let lighty = fromJust (lookup name dirs)
      pid = lighty </> (name ++ ".pid")
  exists <- liftIO $ doesFileExist pid
  if exists
     then do liftIO $ putStrLn caption
             endDaemon pid
     else liftIO $ putStrLn $ "no such file " ++ pid

----------------------------------------
-- `start' command

startCmd :: Cmd
startCmd = Cmd { cmdName = "start"
               , cmdDesc = "Start Lighttpd and FastCGI server"
               , cmdFlags = []
               , cmdAct  = action } where
    action :: [String] -> Command ()
    action _ = do
      initProject
      ensureBuilt
      startProject

startProject :: Command ()
startProject = do
  spawnFCGI
  startLighty

startLighty :: Command ()
startLighty = onlyIfLighttpd $ do
    dirs <- projectDirs
    let cmd = "lighttpd -f " ++ conf
        lighty = fromJust (lookup "lighttpd" dirs)
        conf = lighty </> "lighttpd.conf"
        pid = lighty </> "lighttpd.pid"
    performIfNotExists pid (Just "Lighttpd daemon already running.") $ do
      liftIO $ putStrLn "Spawning Lighttpd daemon ... "
      runShellCmd cmd
      liftIO $ putStrLn "Lighttpd daemon started."

spawnFCGI :: Command ()
spawnFCGI = do
  name <- projectName
  dirs <- projectDirs
  dir  <- projectDir
  out <- gets $ projOutDir . fromJust . kibProject
  let cmd = printf "spawn-fcgi -f %s -s %s -P %s" fcgi sock pid
      fpath = fromJust $ lookup "fastcgi" dirs
      ppath = fromJust $ lookup "public" dirs
      fcgi = dir </> out </> name ++ ".fcgi"
      sock = fpath </> name ++ ".sock"
      pid = fpath </> "fastcgi.pid"
  performIfNotExists pid (Just "FastCGI daemon already running.") $ do
    liftIO $ putStrLn "Spawning FastCGI daemon ... "
    runShellCmd cmd
    liftIO $ putStrLn "Done."

ensureBuilt :: Command ()
ensureBuilt = do
  name <- projectName
  dir  <- projectDir
  out <- gets $ projOutDir . fromJust . kibProject
  main' <- gets $ projMainIs . fromJust . kibProject
  let main = dir </> main'
      fcgi = out </> name ++ ".fcgi"
  fcgiExists <- liftIO $ doesFileExist fcgi
  if fcgiExists
     then do [mainTime,fcgiTime] <- mapM modTime [main,fcgi]
             if mainTime > fcgiTime
                then build
                else return ()
     else build
  where modTime p = liftIO $ modificationTime <$> getFileStatus p
        build = cmdAct buildCmd []

----------------------------------------
-- `help' command

-- | Display help about commands
helpCmd :: Cmd
helpCmd = Cmd { cmdName = "help"
              , cmdDesc = "Help about commands (try `help <command>')"
              , cmdFlags = []
              , cmdAct  = action } where
    action :: [String] -> Command ()
    action []      = cmdList
    action (cmd:_) = cmdHelp cmd

-- | List all the commands
cmdList :: Command ()
cmdList = do
  let longest = foldr1 max $ map (length . cmdName) commands
  appName <- gets kibAppName
  liftIO $ do
    printf "Usage: %s COMMAND [FLAGS]\n\n" appName
    putStrLn "Commands:"
    mapM_ (showHelp longest) commands
    putStrLn "\nTypical step for creating Kibro project:"
    putStrLn "  bolt new [PROJECT_NAME]"
  where showHelp len (Cmd name desc _ _) = do
            printf "%s   %s\n" (fill ' ' len name) desc

-- | Show help for a command
cmdHelp :: String -> Command ()
cmdHelp command = 
    case find ((==command) . cmdName) commands of
      Nothing  -> cmdError $ printf "No such command `%s'" command
      Just cmd -> do 
        appName <- gets kibAppName
        liftIO $ do
          let longest = foldr1 max $ map (length . fst) (cmdFlags cmd)
          printf "Usage: %s %s [FLAGS]\n\n" appName command
          printf "Flags for %s:\n" command
          mapM_ (showFlag longest) (cmdFlags cmd)
          where showFlag len (name,desc) = do
                  printf " --%s   %s\n" (fill ' ' len name) desc

----------------------------------------
-- `build' command

-- | Build the current project
buildCmd :: Cmd
buildCmd = Cmd { cmdName  = "build"
               , cmdDesc  = "Build the current project"
               , cmdFlags = []
               , cmdAct   = action } where
    action :: [String] -> Command ()
    action _ = do
      initProject
      buildProject

buildProject :: Command ()
buildProject = do
  dirs <- projectDirs
  let src = fromJust . lookup "src" $ dirs
      public = fromJust . lookup "public" $ dirs
  dir <- projectDir
  main <- projectMain
  name <- projectName
  opts <- gets $ projGHCOpts . fromJust . kibProject
  let cmd = "ghc --make " ++ dir </> main ++ " -o " ++ fcgi ++ " -threaded " ++ opts
      fcgi = dir </> public </> name ++ ".fcgi"
  curDir <- liftIO $ getCurrentDirectory
  liftIO $ setCurrentDirectory $ dir </> src
  runShellCmd cmd
  liftIO $ setCurrentDirectory curDir

----------------------------------------
-- `new' command

-- | Create a new project
newCmd :: Cmd
newCmd = Cmd { cmdName  = "new"
             , cmdDesc  = "Create a new project"
                          -- TODO
             , cmdFlags = [("start","Start after creating")]
             , cmdAct   = action } where
    action :: [String] -> Command ()
    action []       = cmdError "`new' command needs a project name"
    action (name:_) = do
      case match regex name of
        Nothing -> cmdError $ "invalid project name, should be " ++ regex
        Just _  -> do newProject name; tryMakeProject
      where regex = "^[a-z][a-z0-9_-]+$"

-- | Try to make a project
tryMakeProject :: Command ()
tryMakeProject = do
  makeProjDir
  writeDefMain
  writeLightyConf
  writeProjConfig

-- | Make the project directory structure
makeProjDir :: Command ()
makeProjDir = do
  dir <- projectDir
  dirs <- map snd `fmap` projectDirs
  exists <- liftIO $ doesDirectoryExist dir
  if exists
     then do dir' <- liftIO $ makeRelativeToCurrentDirectory dir
             cmdError $ "directory `" ++ dir' ++ "' already exists"
     else liftIO $ do putStrLn "Creating directory structure ..."
                      mapM_ createDir dirs
                      putStrLn "Finished creating directory structure."
  where createDir dir = do putStrLn $ "  " ++ dir
                           createDirectoryIfMissing True dir

-- | Set the current project in the state
newProject :: String -> Command ()
newProject name = do
  pwd <- liftIO $ getCurrentDirectory
  let project = KibroProject { projName     = name
                             , projDirs     = defaultDirs 
                             , projOutDir   = "public" 
                             , projMainIs   = "src" </> "Main.hs"
                             , projLighttpd = True
                             , projGHCOpts  = "" }
  modify $ \s -> s { kibProjDir = Just $ pwd </> name
                   , kibProject = Just project }

-- | Write default Main.hs file
writeDefMain :: Command ()
writeDefMain = do
  dir <- projectDir
  main <- projectMain
  liftIO $ do
    putStr $ "Writing Main.hs ... "
    writeFile (dir </> main) mainSrc
    putStrLn "done."

-- | Write the lighttpd.conf file
writeLightyConf :: Command ()
writeLightyConf = onlyIfLighttpd $ do
  dir <- projectDir
  dirs <- projectDirs
  conf <- lighttpdDotConf
  case lookup "lighttpd" dirs of
    Nothing   -> cmdError "lighttpd config path not found in project config"
    Just path -> do liftIO $ do
                      putStr $ "Writing lighttpd.conf ... "
                      writeFile (path </> "lighttpd.conf") conf
                      putStrLn "done."
                    writeCustom path

writeCustom :: FilePath -> Command ()
writeCustom path = onlyIfLighttpd $ liftIO $ do
  let custom = path </> "custom.conf"
  exists <- doesFileExist custom
  putStr "Writing custom.conf ... "
  if exists
     then putStrLn "already exists, skipping."
     else do writeFile custom customDotConf
             putStrLn "done."

-- | Write the Kibro configuration to .bolt file
writeProjConfig :: Command ()
writeProjConfig = do
  dir <- projectDir
  name <- projectName
  liftIO $ do
    putStr $ "Writing " ++ name ++ ".bolt ... "
    writeFile (dir </> name ++ ".bolt") $ C.to_string $ buildConfig name
    putStrLn "done."

-- | Initialise the Kibro project by reading the configuration file
initProject :: Command ()
initProject = do
  proj <- gets kibProject
  when (not $ isJust proj) $ do
    pwd <- liftIO $ getCurrentDirectory
    configs <- liftIO $ getDirectoryContents pwd
    let config = filter (isJust . match "^[a-z][a-z0-9_-]+\\.bolt$") configs
    case config of
      [config] -> do liftIO $ putStr $ "Reading config file " ++ config ++ " ... "
                     config' <- readConfig config
                     modify $ \s -> s { kibProject = Just config'
                                      , kibProjDir = Just pwd }
                     liftIO $ putStrLn "done."
      -- TODO: --config option
      []       -> cmdError "no bolt file found. try the `new' command"
      _        -> cmdError "more than one bolt file found"

----------------
-- Command types

-- Throw an error, prefixing the command that threw it to the message
cmdError :: [Char] -> Command a
cmdError err = do
  name <- gets $ cmdName . kibCmd
  throwError $ name ++ ": " ++ err

data Cmd = Cmd 
    { cmdName  :: String
    , cmdDesc  :: String
    , cmdFlags :: [(String,String)]
    , cmdAct   :: [String] -> Command ()
    }
instance Show Cmd where 
    show (Cmd name desc _ _) = "Cmd { cmdName = " ++ show name ++
                               ", cmdDesc = " ++ show desc ++ " }"

type Command = ErrorT String Kibro

----------------
-- Kibro types

-- | Kibro monad
type Kibro = StateT KibroSt IO

-- | Kibro running state
data KibroSt = KibroSt 
    { kibProject :: Maybe KibroProject
    , kibProjDir :: Maybe FilePath
    , kibCmd     :: Cmd
    , kibArgs    :: [String]
    , kibAppName :: String
    } deriving Show

-- Helper functions
projectMain = gets $ projMainIs . fromJust . kibProject
projectName = gets $ projName . fromJust . kibProject
projectDir  = gets $ fromJust . kibProjDir
projectLighttpd = gets $ projLighttpd . fromJust . kibProject
onlyIfLighttpd :: Command () -> Command ()
onlyIfLighttpd m = do doIt <- projectLighttpd
                      when doIt m
projectDirs = do 
  dir <- projectDir
  gets (map (second (dir </>)) . projDirs . fromJust . kibProject)

-- | Project type
data KibroProject = KibroProject 
    { projName     :: String 
    , projDirs     :: [(String,FilePath)]
    , projOutDir   :: FilePath
    , projMainIs   :: FilePath
    , projLighttpd :: Bool
    , projGHCOpts  :: String
    } deriving (Eq,Show,Read)

----------------
-- Configuration parser and shower

buildConfig :: String -> ConfigParser
buildConfig name = fromEither $ foldM setOpt C.emptyCP (addName $ opts) where
    setOpt cp (name,opt) = C.set cp "DEFAULT" name opt
    fromEither (Right x) = x
    addName = (("name",name) :) . filter ((/="name") . fst)

readConfig :: String -> Command KibroProject
readConfig config = do   
  rv <- runErrorT $
    do cp <- join $ liftIO $ C.readfile C.emptyCP config
       let x = cp
       options <- mapM (\e -> (,) e <$> C.get x "DEFAULT" e) optNames
       return (options :: [(String,String)])
  case rv of
    Left (C.NoOption entry, _) -> configError $ "`" ++ entry ++ "' was not provided in the config"
    Left (e,_)                 -> configError $ show e
    Right vs                   -> return $ optsToProject vs
  where configError e = cmdError $ config ++ ": " ++ e

optsToProject :: [(String,String)] -> KibroProject
optsToProject ls = KibroProject 
                   { projName     = get "name"
                   , projDirs     = [("fastcgi",get "fastcgi-dir")
                                    ,("app", get "app-dir")
                                    ,("lighttpd",get "lighttpd-dir")
                                    ,("public", get "public-dir")
                                    ,("src", get "src-dir")]
                   , projOutDir   = get "fcgi-out-dir" 
                   , projMainIs   = get "main-is"
                   , projLighttpd = readBool $ get "manage-lighttpd" 
                   , projGHCOpts  = get "ghc-options" }
    where get = fromJust . flip lookup ls
          readBool = readBool' . map toLower
          readBool' "yes" = True
          readBool' "no"  = False
          readBool' "true" = True
          readBool' "false" = False

optNames = map fst opts
opts = [("name","jboborei")
       ,("app-dir","app")
       ,("lighttpd-dir", "app/lighttpd")
       ,("fastcgi-dir", "app/fastcgi")
       ,("public-dir", "public")
       ,("src-dir", "src")
       ,("fcgi-out-dir", "public")
       ,("main-is", "src/Main.hs")
       ,("ghc-options", "")
       ,("manage-lighttpd", "yes")]

-----------------------------------------------------------------------------
-- Default values

mainSrc :: String
mainSrc = "module Main (main) where\n\
          \import Bolt\n\
          \import Bolt.Shared\n\
          \ \n\
          \import qualified Bolt.Plugin.HStringTemplate as PHStringTemplate\n\
          \import qualified Bolt.Plugin.Routing as PRouting\n\
          \import qualified Bolt.Plugin.Session as PSession\n\
          \ \n\
          \-- Lightning Bolts came out of the sky!!!\n\
          \ \n\
          \main\n\
          \  = do bStartBolt\n\
          \         -- Define plugins here\n\
          \         [PSession.plugin\n\
          \         ,PHStringTemplate.plugin True \"tmpl/\"\n\
          \         ] \n\
          \         -- Define routes here\n\
          \         [\n\
          \          (\".\", index)\n\
          \          --,(\"\", function)\n\
          \         ]\n\
          \ \n\
          \       return ()\n\
          \ \n\
          \index = bshOutput \"Lightning bolts came out of the sky!!!\"\n"


lighttpdDotConf :: Command String
lighttpdDotConf = do
  root <- projectDir
  name <- projectName
  dirs <- gets $ projDirs . fromJust . kibProject
  let appDir = fromJust $ lookup "app" dirs
      fastCGIDir = fromJust $ lookup "fastcgi" dirs
      lightDir = fromJust $ lookup "lighttpd" dirs
      pubDir = fromJust $ lookup "public" dirs
  return $ showSettings
    [("Do not modify this file. You should modify custom.conf" #)
    ,"var.k_base_dir"      .=. dir root
    ,"var.k_app_dir"       .=. var "k_base_dir" <+> dir appDir
    ,"var.k_fastcgi_dir"   .=. var "k_base_dir" <+> dir fastCGIDir
    ,"var.k_lighttpd_dir"  .=. var "k_base_dir" <+> dir lightDir
    ,"var.k_public_dir"    .=. var "k_base_dir" <+> dir pubDir
    ,"var.k_fcgi_filename" .=. name ++ ".fcgi"
    ,"var.k_fcgi_path"     .=. var "k_public_dir" <+> var "k_fcgi_filename"
    ,"var.k_socket_path"   .=. var "k_fastcgi_dir" <+> name ++ ".sock"
    ,"var.k_error_log"     .=. var "k_lighttpd_dir" <+> "error.log"
    ,"var.k_port"          .=. (3000 :: Int)
    ,("This value must not be changed, Kibro depends on it for stopping" #)
    ,"server.pid-file"     .=. var "k_lighttpd_dir" <+> "lighttpd.pid"
    ,include "custom.conf"]
                  where dir = init . (</> " ")

customDotConf :: String
customDotConf = showSettings
  [("Only change these if you know what you are doing" #)
  ,"fastcgi.server" .=. [var "k_fcgi_path" .=>. [["socket" .=>. var "k_socket_path"]]]
  ,"server.error-handler-404" .=. var "k_fcgi_filename"
  ,"index-file.names" .=. [var "k_fcgi_filename"]
  ,("Shouldn't need to change these" #)
  ,"server.document-root" .=. var "k_public_dir"
  ,"server.errorlog" .=. var "k_error_log"
  ,("Feel free" #)
  ,"server.modules" .=. ["mod_rewrite","mod_redirect","mod_fastcgi"]
  ,"server.port" .=. var "k_port"
  ,"server.dir-listing" .=."enable"
  ,"dir-listing.encoding" .=."utf-8"
  ,"mimetype.assign" .=. ["" .=>. "text/plain"]]

-- | Default directory structure
defaultDirs :: [(String,FilePath)]
defaultDirs =
    [("app","app")
    ,("lighttpd","app" </> "lighttpd")
    ,("fastcgi","app" </> "fastcgi")
    ,("public","public")
    ,("src","src")]

------------------------------------------------------------------------------
-- Lighttpd config

-- | Class for generalising the properties.
class LightyProperty a where toProp :: a -> Prop

-- | Necessary instances.
instance LightyProperty [[Char]] where toProp = List
instance LightyProperty [Char] where toProp = String
instance LightyProperty Int where toProp = Number
instance LightyProperty [Prop] where toProp = Props
instance LightyProperty [[Prop]] where toProp = Props . map Props
instance LightyProperty Prop where toProp = id

-- | A property.
data Prop = String String
          | Number Int
          | List [String]
          | Assign Prop Prop
          | Variable String
          | Props [Prop]
          | Concat Prop Prop
-- | A property "setting".
data LightyLine = Set String Prop | Comment String | Include String

instance Show Prop where
    show (String s) = show s
    show (Number n) = show n
    show (List xs)  = "(" ++ commasShow xs ++ ")" where
    show (Assign p v) = show p ++ " => " ++ show v
    show (Props ps) = "(" ++ commasShow ps ++ ")"
    show (Variable v) = v
    show (Concat p1 p2) = show p1 ++ " + " ++ show p2

instance Show LightyLine where
    show (Set s p) = s ++ " = " ++ show p
    show (Comment s) = "# " ++ s
    show (Include s) = "include " ++ show s

-- | Print a list of settings in the lighttpd config format.
showSettings :: [LightyLine] -> String
showSettings = unlines . map show

commasShow :: Show a => [a] -> String
commasShow = commas . map show

---------------------------------------
-- Combinators

-- | "Set" a property.
(.=.) :: LightyProperty a => String -> a -> LightyLine
p .=. v = Set p (toProp v)
infixr 0 .=.

-- | "Assign" a value to something.
(.=>.) :: (LightyProperty a,LightyProperty b) => a -> b -> Prop
(.=>.) a b = Assign (toProp a) (toProp b)
infixr 0 .=>.

var = Variable
(<+>) :: (LightyProperty a,LightyProperty b) => a -> b -> Prop
a <+> b = Concat (toProp a) (toProp b)
infixr 1 <+>
(#) = Comment
include = Include

------------------------------------------------------------------------------
-- Generic utilities

-- Perform some action if a given file doesn't exist, otherwise print message
performIfNotExists :: FilePath -> Maybe String -> Command () -> Command ()
performIfNotExists path msg m = do
  exists <- liftIO $ doesFileExist path
  if not exists then m else maybe (return ()) (liftIO . putStrLn) msg

-- Run shell command
runShellCmd :: String -> Command ()
runShellCmd cmd = do
  id <- liftIO $ runCommand cmd
  status <- liftIO $ waitForProcess id
  case status of
    ExitSuccess   -> return ()
    ExitFailure _ -> cmdError $ "the following command failed:\n  " ++ cmd

-- Stricter command runner
runShellCmd' :: String -> Command ()
runShellCmd' cmd = do
  out <- liftIO $ run cmd ""
  case out of
    Right (ExitSuccess,err,_) | null err  -> return ()
                              | otherwise -> fail err
    Right (_,err,_) -> fail err
    Left err -> fail err
  where fail err = cmdError $ "there were errors with the \
                              \following command:\n  " ++ cmd ++ "\n\n" ++ 
                              err

-- Silent runner
runShellCmdSilent :: String -> Command ()
runShellCmdSilent cmd = do
  liftIO $ run cmd ""
  return ()

-- Big-ass-but-stable process launcher
run :: String -> String -> IO (Either String (ExitCode,String,String))
run cmd input = do
  pipe <- catch (Right `fmap` runInteractiveCommand cmd)
                (const $ return $ Left "")
  case pipe of
    Right (inp,out,err,pid) -> do
                  catch (do hSetBuffering inp NoBuffering
                            hPutStr inp input 
                            hClose inp
                            errv <- newEmptyMVar
                            outv <- newEmptyMVar
                            output <- hGetContents out
                            errput <- hGetContents err
                            forkIO $ evaluate (length output) >> putMVar outv ()
                            forkIO $ evaluate (length errput) >> putMVar errv ()
                            takeMVar errv
                            takeMVar outv
                            e <- catch (waitForProcess pid)
                                       (const $ return $ ExitFailure 1)
                            return $ Right (e,errput,output))
                        (const $ return $ Left "Broken pipe")
    _ -> return $ Left "Unable to launch process"

fill c len text = take len $ text ++ repeat c
match = matchRegex . mkRegex
commas = concat . intersperse ","

endDaemon pid = do runShellCmdSilent $ "kill `cat \"" ++ pid ++ "\"`"
                   runShellCmdSilent $ "rm \"" ++ pid ++ "\""
                   return ()
