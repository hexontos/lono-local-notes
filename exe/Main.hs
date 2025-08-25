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


let defaultConfigPath = "$XDG_CONFIG_HOME/lono/config"
let alternativeConfigPath = "$HOME/.config/lono/config"
let tmpPath = "/tmp/.temp_note_of_lono"
let defaultDBPath = "$HOME/.local/lono/db.json"

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

getConfigPath :: CONFIG_PATH
getConfigPath = if doesPathExist defaultConfigPath then defaultConfigPath else alternativeConfigPath


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


createDB :: FilePath -> IO (NoteDB) -- filePath is DB_PATH
createDB path = b.writeFile path (encodePretty NoteDb)


loadDB :: FilePath -> IO (NoteDB)


updateDB :: NoteDB -> FilePath -> IO ()


writeTmpNoteAsFile :: NoteDB -> IO ()
writeTmpNoteAsFile db = B.writeFile tmpPath (encodePretty db)


-- updateTmpNoteFileIntoDB


-- deleteTmpNoteFile




--------- ENCRYPTION (later)

--------- APP GUI INTERFACE (later)

--------- APP INTERFACE

ls :: ARGS -> IO () -- ls -all
ls [] = -- will print all notes in current path (listDirectory) 
ls "./" = -- will print all known notes in current and sub-folders
ls "/" = -- will print all notes paths
ls x = raise Error 

mv :: ARGS -> IO ()
mv (selectedNote:newNotePath:[]) = -- need to have correct paths (figure out paths)
mv x = raise Error

rm :: ARGS -> IO ()
rm [] = putStrLn "No name/path of note was provided."
rm (note_path:[]) = note_path
rm x = raiseError

chosenArg :: FilePath -> IO () -- so basically we should only get one arg which should be name of note (path). Make sure the path is correct. Load DB from json file, create tmp file, cmd terminal editor, update DB. Update DB file (json).
chosenArg

setupArg :: String -> IO () 
setupArg cli_args =
 | len == 1 = if (head cli_args) == "--create-db" then 
 | len == 2 = f (head cli_args) (last cli_args)
 | otherwise = _
 where
     len = length cli_args
     f "--set-editor" p = updateConfigValue "EDITOR" p
     f "--set-db" p = if doesPathExist p then updateConfigValue "DB_PATH" p else raise Error
     f _ _ = _


main :: IO ()
main = do 
    config_path <- getConfigPath
    when (config_path == "") $ createConfigurationFile

    args <- getArgs
    setupArg args

    (editor, db_path) <- loadConfigurationFile config_path
    when (editor == "") $ putStrLn "Editor is not set. Run: `lono --set-editor <editor of your choosing>"
    when (db_path == "") $ putStrLn "Database of notes is not set. Either create it with `lono --create-db` or set existing one with `lono --set-db <path to db.json>`."

    when (doesFileExist db_path) $ raise Error

    --when (length args == 0) $ startTerminalGUI
    --when (length args == 1) $ singleArg (head args)
    first_arg <- head args
    when ("ls" == first_arg) $ ls args
    when ("mv" == first_arg) $ mv args -- basically rename
    when ("rm" == first_arg) $ rm args

    when (length args == 1) $ chosenNote (args !! 1) 



    


