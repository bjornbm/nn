{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Strict #-}

{-

Global options:

    --home NN_HOME
    --editor NN_EDITOR
    --dry-run

Multiple note selection:

   [-i ID] [-t TAG] [--name TERMS] [SEARCH TERMS]

Multiple section commands:

    nn list
    nn path
    nn cat
    nn exec
    nn edit
    nn delete
    nn retag
    nn obsolete
    nn pretty
    nn pandoc

Single note selection:

    -i ID

Single note commands:

    nn redate -i ID (time)
    nn rename -i ID NAME
    nn copy -i ID NAME   # nn cat -i ID | pbcopy

Note creation commands:

    nn new (-i ID) TAG NAME
    nn import TAG (--title TITLE) FILE
    nn paste (-i ID) TAG NAME   # pbpaste | nn new TAG NAME

Sanity checking commands:

    nn check
    nn stats

-}


-- TODO creat Types module?


module Options where

import Data.Bool (bool)
import Options.Applicative

import Util (SearchTool (..))

data Options = Options
  { optionNnHome     :: FilePath
  , optionSearchTool :: SearchTool
  -- TODO other global options
  , optionsCommand :: Command
  }

data Command
  = List     { path :: Bool, exec :: Maybe String, mselection :: SelectMany }
  | Cat      { noheaders :: Bool, mselection :: SelectMany }
  | Edit     { mselection :: SelectMany }
  | Tags     { popularity :: Bool }
  | Check    { names :: Bool, references :: Bool }
  | Import   { modID :: Bool, newID :: Maybe String, title :: Maybe String, newTag :: String, files :: [String] }
  | New      { empty :: Bool, newTag :: String, nameParts :: [String] }
  | Obsolete { dryrun :: Run, mselection :: SelectMany }
  | Rename   { dryrun :: Run, selection :: SelectOne, nameParts :: [String] }
  | Retag    { dryrun :: Run, newTag :: String, mselection :: SelectMany }
  | ChangeID { dryrun :: Run, newID :: Maybe String, selection :: SelectOne }
  deriving (Show) -- , Data, Typeable)

data Run = Dry | Full deriving (Show, Eq)

data Join = AND | OR deriving (Show, Eq)

data SelectMany = SelectMany
  { selectManyLast :: Bool
  , selectManyIDs :: [String]
  , selectManyTags :: [String]
  -- , selectManyEXTs :: [String]
  -- , selectManyAll :: Bool
  -- , selectManyJoin :: Join
  , selectManyTerms :: [String]
  } deriving (Show, Eq)

selectManyOptions :: Parser SelectMany
selectManyOptions = do
  selectManyLast <- lastP
  selectManyIDs <- idsP
  selectManyTags <- tagsP
  -- <*> selectExts
  -- <*> selectAll
  -- <*> selectJoin
  selectManyTerms <- selectTerms
  pure SelectMany {..}
  where
    lastP  = switch (lsh "last" 'l' "Select the most recent note")
    idsP   = many $ strOption
      (lsh "id" 'i' "The ID of a note to select" <> metavar "ID")
    tagsP  = many $ strOption
      (lsh "tag"  't' "Select notes tagged with TAG" <> metavar "TAG")
    -- selectExts  = many $ strOption
    --  (lsh "ext" 'e' "Select notes with extension EXT" <> metavar "EXT")
    -- selectAll   = switch (lh "all" "Include obsoleted notes in selection")
    -- selectJoin  = (\b -> if b then AND else OR) <$> switch (lh "and" "Select only notes satisfying ALL specified criteria. If not specified notes satisfying ANY criteria will be selected (except SEARCH TERMS which must all be satisfied).")
    selectTerms = manyArguments "SEARCH TERMS"


data SelectOne = SelectID { selectOneID :: String } | SelectLast deriving (Eq, Show)

selectOneOptions :: Parser SelectOne
selectOneOptions = maybe SelectLast SelectID <$> idP
  where
    idP = strOptional (lsh "id" 'i' "The ID of the note to select. If no ID is specified the most recent (non-obsolete) note is selected. If several notes share the same ID (see `nn check`) only the first is selected." <> metavar "ID")



infoh :: Parser a -> InfoMod a -> ParserInfo a
infoh parser = info (helper <*> parser)

commandh :: String -> Parser a -> InfoMod a -> Mod CommandFields a
commandh name parser = command name . infoh parser

commandhd :: String -> Parser a -> String -> Mod CommandFields a
commandhd name parser = command name . infoh parser . progDesc

strOptional :: Mod OptionFields String -> Parser (Maybe String)
strOptional = optional . strOption

lh :: HasName f => String -> String -> Mod f a
lh l h = long l <> help h

lsh :: HasName f => String -> Char -> String -> Mod f a
lsh l s h = long l <> short s <> help h

manyArguments :: String -> Parser [String]
manyArguments = many . argument str . metavar

someArguments :: String -> Parser [String]
someArguments = some . argument str . metavar

options :: Parser Command
options = subparser
  (  commandhd "list"         listOptions "List selected notes"
  <> commandhd "cat"           catOptions "Concatenate notes to STDOUT"
  <> commandhd "edit"         editOptions "Edit notes selected by ID or search terms"
  <> commandhd "tags"         tagsOptions "Display all tags currently in use"
  <> commandhd "check"       checkOptions "Sanity check notes"
  <> commandhd "import"     importOptions "Import a file as a note"
  <> commandhd "new"           newOptions "Create a new note"
  <> commandhd "obsolete" obsoleteOptions "Mark notes as obsolete"
  <> commandhd "rename"     renameOptions "Change name of selected note"
  <> commandhd "retag"       retagOptions "Change tag of note"
  <> commandhd "changeid" changeIDOptions "Change ID of selected note"  -- ID -> timestamp?
  ) <|> listOptions

listOptions :: Parser Command
listOptions = List
  <$> switch      (lsh "path" 'p' "List full paths of note files")
  <*> strOptional (lsh "exec" 'e' "Pass notes file paths as arguments to COMMAND" <> metavar "COMMAND")
  <*> selectManyOptions

catOptions :: Parser Command
catOptions = Cat
  <$> switch       (lsh "noheaders" 'n' "Do not include headers in output")
  <*> selectManyOptions

editOptions :: Parser Command
editOptions = Edit
  <$> selectManyOptions

tagsOptions :: Parser Command
tagsOptions = Tags
  <$> switch (lsh "popularity" 'p' "Show and sort tags by popularity")

checkOptions :: Parser Command
checkOptions = do
  names      <- switch (lsh "names"      'n' "List badly named notes")
  references <- switch (lsh "references" 'r' "List notes containing bad note references")
  pure Check {..}

importOptions :: Parser Command
importOptions = do
  modID  <- switch       (lsh "modificationtime" 'm' "Assign an ID to the note based on the file's modification time. If the ID is already in use the next available ID will be assigned to the note.")
  newID  <- strOptional  (lsh "id" 'i' "The ID to assign to the note (takes precedence over the `-m` switch). If no ID is specified the current time will be used. If the ID is already in use the next available ID will be assigned to the note." <> metavar "ID")
  title  <- strOptional  (lsh "title" 't' titleDesc <> metavar "TITLE")
  newTag <- argument str (metavar "TAG")
  files  <- someArguments "FILES"
  pure Import {..}
  where
    titleDesc = "Import with title TITLE. If no title is specified the name of the file will be used as the note title."

dryswitch :: Parser Run
dryswitch = bool Full Dry <$> switch (long "dry-run" <> help "Show how the files would be renamed, but don't actually do anything")

newOptions :: Parser Command
newOptions = New
  <$> switch (lsh "empty" 'e' "Create an empty note")
  <*> argument str (metavar "TAG")
  <*> someArguments "NAME"  -- TODO: should be TITLE?

obsoleteOptions :: Parser Command
obsoleteOptions = Obsolete
  <$> dryswitch
  <*> selectManyOptions

renameOptions :: Parser Command
renameOptions = Rename
  <$> dryswitch
  <*> selectOneOptions
  <*> someArguments "NAME"

retagOptions :: Parser Command
retagOptions = Retag
  <$> dryswitch
  <*> argument str (metavar "TAG")
  <*> selectManyOptions

changeIDOptions :: Parser Command
changeIDOptions = ChangeID
  <$> dryswitch
  <*> strOptional (lsh "newid" 'n' "The new ID to assign to the note. If no new ID is specified the current time will be used. If the new ID is already in use the next available ID will be assigned to the note." <> metavar "NEW ID")
  <*> selectOneOptions

descText :: String
descText = "nn is a tool for conveniently and efficiently creating, searching, and displaying notes. "
        <> "Different behaviors are invoked by different subcommands. "
        <> "The default behavior, when no subcommand is passed, is to search for notes. "
        <> "nn will create and search for notes in the directory identified by the environment variable $NN_HOME (absolute path)."

description :: InfoMod a
description = fullDesc
    <> header "nn - a note management tool"
    <> progDesc descText
    <> footer "(c) 2016–2018, Bjorn Buckwalter"

parseCommand :: IO Command
parseCommand = execParser (info (helper <*> options) description)
