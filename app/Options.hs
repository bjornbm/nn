{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

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


import Data.Maybe (maybe)
import Data.Semigroup ((<>))
import Options.Applicative


data Options = Options
  { nnHome     :: FilePath
  -- TODO other global options
  , optCommand :: Command
  }

data Command
  = List     { path :: Bool, exec :: Maybe String, mselection :: SelectMany }
  | Cat      { noheaders :: Bool, id :: String }
  | Edit     { editID :: Maybe String, terms :: [String] }
  | Tags     { popularity :: Bool }
  | Check    { names :: Bool, references :: Bool }
  | Import   { title :: Maybe String, newTag :: String, file :: String }
  | New      { empty :: Bool, newTag :: String, nameParts :: [String] }
  | None     { terms :: [String] }
  | Obsolete { dryrun :: Run, id :: String }
  | Rename   { dryrun :: Run, selection :: SelectOne, nameParts :: [String] }
  | Retag    { dryrun :: Run, id :: String, newTag :: String }
  deriving (Show) -- , Data, Typeable)

data Run = Dry | Full deriving (Show, Eq)

data Join = AND | OR deriving (Show, Eq)

data SelectMany = SelectMany
  { sLast :: Bool
  , sIDs :: [String]
  , sTAGs :: [String]
  , sEXTs :: [String]
  , sAll :: Bool
  , sJoin :: Join
  , sTERMs :: [String]
  } deriving (Show, Eq)

selectManyOptions = SelectMany
  <$> selectLast
  <*> selectIDs
  <*> selectTags
  <*> selectExts
  <*> selectAll
  <*> selectJoin
  <*> selectTerms
  where
    selectLast  = switch (lsh "last" 'l' "Select the most recent note")
    selectIDs   = many $ strOption
      (lsh "id" 'i' "The ID of a note to select" <> metavar "ID")
    selectTags  = many $ strOption
      (lsh "tag"  't' "Select notes tagged with TAG" <> metavar "TAG")
    selectExts  = many $ strOption
      (lsh "ext" 'e' "Select notes with extension EXT" <> metavar "EXT")
    selectAll   = switch (lh "all" "Include obsoleted notes in selection")
    selectJoin  = (\b -> if b then AND else OR) <$> switch (lh "and" "Select only notes satisfying ALL specified criteria. If not specified notes satisfying ANY criteria will be selected (except SEARCH TERMS which must all be satisfied).")
    selectTerms = manyArguments "SEARCH TERMS"


data SelectOne = SelectID { sID :: String } | SelectLast deriving (Eq, Show)

selectOneOptions = maybe SelectLast SelectID <$> selectID
  where
    selectID = strOptional (lsh "id" 'i' ("The ID of the note to select. If no ID is specified the most recent (non-obsolete) note is selected. If several notes share the same ID (see `nn check`) only the first is selected.") <> metavar "ID")




infoh parser = info (helper <*> parser)
commandh name parser = command name . infoh parser
commandhd name parser = command name . infoh parser . progDesc
strOptional = optional . strOption
lh l h = long l <> help h
lsh l s h = long l <> short s <> help h
manyArguments = many . argument str . metavar
someArguments = some . argument str . metavar

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
  ) <|> (None <$> manyArguments "SEARCH TERMS")

listOptions = List
  <$> switch      (lsh "path" 'p' "List full paths of note files")
  <*> strOptional (lsh "exec" 'e' "Pass notes file paths as arguments to COMMAND" <> metavar "COMMAND")
  <*> selectManyOptions

catOptions :: Parser Command
catOptions = Cat
  <$> switch       (lsh "noheaders" 'n' "Do not include headers in output")
  <*> argument str (help "The ID of the note to cat" <> metavar "ID")

editOptions = Edit
  <$> strOptional (lsh "id" 'i' "The ID of the note to edit. If no ID or terms are specified the most recent note is selected." <> metavar "ID")
  <*> manyArguments "SEARCH TERMS"

tagsOptions = Tags
  <$> switch (lsh "popularity" 'p' "Show and sort tags by popularity")

checkOptions = Check
  <$> switch (lsh "names"      'n' "List badly named notes")
  <*> switch (lsh "references" 'r' "List notes containing bad note references")

importOptions = Import
  <$> strOptional  (lsh "title" 't' titleDesc <> metavar "TITLE")
  <*> argument str (metavar "TAG")
  <*> argument str (metavar "FILE")
  where
    titleDesc = "Import with title TITLE. If no title is specified the name of the file will be used as the note title."

dryswitch = dryrun <$> switch (long "dry-run" <> help "Show how the files would be renamed, but don't actually do anything")
  where
    dryrun True  = Dry
    dryrun False = Full

newOptions = New
  <$> switch (lsh "empty" 'e' "Create an empty note")
  <*> argument str (metavar "TAG")
  <*> someArguments "NAME"  -- TODO: should be TITLE?

obsoleteOptions = Obsolete
  <$> dryswitch
  <*> strOption (lsh "id" 'i' "The ID of the note to mark as obsolete" <> metavar "ID")

renameOptions = Rename
  <$> dryswitch
  <*> selectOneOptions
  <*> someArguments "NAME"

retagOptions = Retag
  <$> dryswitch
  <*> strOption (lsh "id" 'i' "The ID of the note to retag" <> metavar "ID")
  <*> argument str (metavar "TAG")

descText = "nn is a tool for conveniently and efficiently creating, searching, and displaying notes. "
        <> "Different behaviors are invoked by different subcommands. "
        <> "The default behavior, when no subcommand is passed, is to search for notes. "
        <> "nn will create and search for notes in the directory identified by the environment variable $NN_HOME (absolute path)."

description = fullDesc
    <> header "nn - a note management tool"
    <> progDesc descText
    <> footer "(c) 2016–2018, Bjorn Buckwalter"

parseCommand :: IO Command
parseCommand = execParser (info (helper <*> options) description)
