import System.Directory (doesPathExist, listDirectory, doesFileExist)
import System.Environment (getArgs)
import System.IO
import Data.List (isPrefixOf)
import Data.List (foldl')
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T



--------- INTERNAL


--let defaultConfigPath = "$XDG_CONFIG_HOME/lono/config"
--let alternativeConfigPath = "$HOME/.config/lono/config"
--let tmpPath = "/tmp/.temp_note_of_lono"
--let defaultDBPath = "$HOME/.local/lono/db.json"

type NotePath = FilePath -- FilePath is String
type EDITOR = String
type DP_PATH = String
type CONFIG_PATH = String
type ARGS = [String]
type NoteDB = HM.HashMap Text NoteValue

data NoteValue = 
    FileContent Text        -- Content of a file
  | Directory NoteDB        -- Nested directory
  deriving (Show, Eq)

-- JSON instances for serialization/deserialization
instance ToJSON NoteValue where
    toJSON (FileContent content) = String content
    toJSON (Directory db) = Object (fmap toJSON db)

instance FromJSON NoteValue where
    parseJSON (String s) = return $ FileContent s
    parseJSON (Object o) = Directory <$> traverse parseJSON o
    parseJSON _ = fail "Invalid JSON structure for NoteValue"


-- figureOutPath :: String -> IO (String) -- for example if user do /folder1/folder2/.../note or just note (so adding current path) or ./folderx/note (so we add current path to it from left)


defaultConfigPath :: IO FilePath
defaultConfigPath = do
  xdgConfig <- lookupEnv "XDG_CONFIG_HOME"
  case xdgConfig of
    Just path -> return (path </> "lono" </> "config")
    Nothing -> do
      home <- lookupEnv "HOME"
      case home of
        Just path -> return (path </> ".config" </> "lono" </> "config")
        Nothing -> error "HOME environment variable not set"


alternativeConfigPath :: IO FilePath
alternativeConfigPath = do
  home <- lookupEnv "HOME"
  case home of
    Just path -> return (path </> ".lono" </> "config")
    Nothing -> error "HOME environment variable not set"


tmpPath :: FilePath
tmpPath = "/tmp/.temp_note_of_lono"


defaultDBPath :: IO FilePath
defaultDBPath = do
  home <- lookupEnv "HOME"
  case home of
    Just path -> return (path </> ".local" </> "lono" </> "db.json")
    Nothing -> error "HOME environment variable not set"


createConfigurationFile :: IO ()
createConfigurationFile = writeFile path "EDITOR  \nDB_PATH  \n"
 where
     path = if doesPathExist defaultPath then defaultPath else alternativePath


loadConfigurationFile :: CONFIG_PATH -> IO (EDITOR, DB_PATH)
loadConfigurationFile path = f "" "" (lines $ readFile path)
 where
     f s1 s2 [] = (s1, s2)
     f s1 s2 (line:rest)
      | "EDITOR" `isPrefixOf` line = f newS s2 rest
      | "DB_PATH" `isPrefixOf` line = f s1 newS rest
      | otherwise = f s1 s2 rest
       where
           newS = foldl' (++) "" (tail $ words line))


updateConfigValue :: FilePath -> String -> String -> IO ()
updateConfigValue configPath key newValue = writeFile configPath (unlines $ map (updateLine key newValue) (lines $ readFile configPath))
 where
    updateLine :: String -> String -> String -> String
    updateLine targetKey newVal line
        | targetKey `isPrefixOf` line = targetKey ++ " " ++ newVal
        | otherwise = line

---------- DATABASE


createDB :: FilePath -> IO ()
createDB path = B.writeFile path (encodePretty $ Directory HM.empty)


loadDB :: FilePath -> IO NoteValue
loadDB path = case eitherDecode (B.readFile path) of
    Left err -> error $ "Failed to parse DB: " ++ err
    Right db -> return db


updateDB :: NoteValue -> FilePath -> IO ()
updateDB db path = B.writeFile path (encodePretty db)


-- tmpPath is global variable
writeTmpNoteAsFile :: Text -> IO ()
writeTmpNoteAsFile content = TIO.writeFile tmpPath content


readTmpNoteFile :: IO Text
readTmpNoteFile = TIO.readFile tmpPath


deleteTmpNoteFile :: IO ()
deleteTmpNoteFile = removeFile tmpPath `catch` (\_ -> return ()) -- ignore if file doesn't exist



--------- ENCRYPTION (later)

--------- APP GUI INTERFACE (later)

--------- APP INTERFACE

ls :: ARGS -> IO () -- ls -all
ls [] = putStrLn "~"-- will print all notes in current path (listDirectory)
ls "./" = putStrLn "~"--- will print all known notes in current and sub-folders
ls "/" = putStrLn "~"-- will print all notes paths
ls x = error "error"


mv :: ARGS -> IO ()
mv (selectedNote:newNotePath:[]) = -putStrLn "~"-- need to have correct paths (figure out paths)
mv x = error "error"


rm :: ARGS -> IO ()
rm [] = putStrLn "No name/path of note was provided."
rm (note_path:[]) = -putStrLn "~"--note_path
rm x = error "error"


chosenArg :: FilePath -> IO () -- so basically we should only get one arg which should be name of note (path). Make sure the path is correct. Load DB from json file, create tmp file, cmd terminal editor, update DB. Update DB file (json).
chosenArg notePath editor dbPath = do
  putStrLn $ "Editing: " ++ notePath
  -- 1. Load the entire database
  -- currentDB <- loadDB dbPath
  -- 2. Look up the current content of 'notePath' in the DB
  -- 3. Write that content to tmpPath (or empty string if new file)
  -- 4. Run the editor: System.Process.callCommand (editor ++ " " ++ tmpPath)
  -- 5. Read the content back from tmpPath
  -- 6. Update the DB HashMap with the new content at the key (T.pack notePath)
  -- 7. Save the DB back to file
  -- 8. Delete the tmp file
  putStrLn "chosenArg not yet implemented"


setupArg :: [String] -> IO ()
setupArg args = case args of
  ["--create-db"] -> do
    dbPath <- defaultDBPath
    createDB dbPath
    putStrLn $ "Created new database at: " ++ dbPath
  ["--set-editor", editorPath] -> do
    configPath <- getConfigPath
    updateConfigValue configPath "EDITOR" editorPath
  ["--set-db", dbPath] -> do
    exists <- doesPathExist dbPath
    if exists
      then do
        configPath <- getConfigPath
        updateConfigValue configPath "DB_PATH" dbPath
      else error "Database path does not exist."
  _ -> putStrLn "Unknown setup command."

    
main :: IO ()
main = do
  configPath <- getConfigPath
  configExists <- doesFileExist configPath
  when (not configExists) $ createConfigurationFile

  args <- getArgs

  -- Check for setup commands first
  case args of
    ("--create-db":_) -> setupArg args >> return ()
    ("--set-editor":_) -> setupArg args >> return ()
    ("--set-db":_) -> setupArg args >> return ()
    _ -> return () -- Continue with normal app flow

  -- Only load config and run the app if it wasn't a setup command
  (editor, db_path) <- loadConfigurationFile configPath

  when (null editor) $ putStrLn "Editor is not set. Run: `lono --set-editor <editor>`"
  when (null db_path) $ putStrLn "Database path is not set. Run: `lono --set-db <path>`"

  dbExists <- doesFileExist db_path
  when (not dbExists) $ error "Database file does not exist. Create it with `lono --create-db`."

  case args of
    [] -> putStrLn "Terminal GUI not implemented yet. Please provide a command." -- startTerminalGUI
    ["ls"] -> ls []
    ("ls":rest) -> ls rest
    ("mv":rest) -> mv rest
    ("rm":rest) -> rm rest
    [noteName] -> chosenArg noteName editor db_path
    _ -> error "Invalid command or too many arguments."

