{-# LANGUAGE DisambiguateRecordFields #-}

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


import Data.Semigroup ((<>))
import Options.Applicative


data Options = Options
  { nnHome     :: FilePath
  -- TODO other global options
  , optCommand :: Command
  }


data Command
  = List     { all :: Bool, path :: Bool, exec :: Maybe String, tagged :: Maybe String, terms :: [String] }
  | Cat      { noheaders :: Bool, id :: String }
  | Edit     { editID :: Maybe String, terms :: [String] }
  | Tags     { popularity :: Bool }
  | Check    { names :: Bool, references :: Bool }
  | Import   { title :: Maybe String, tag :: String, file :: String }
  | New      { empty :: Bool, tag :: String, name :: [String] }
  | None     { terms :: [String] }
  | Obsolete { dryrun :: Bool, id :: String }
  | Rename   { dryrun :: Bool, id :: String, name :: [String] }
  deriving (Show) -- , Data, Typeable)



infoh parser = info (helper <*> parser)
commandh name parser = command name . infoh parser
commandhd name parser = command name . infoh parser . progDesc
strOptional = optional . strOption
lsh l s h = long l <> short s <> help h
manyArguments = many . argument str . metavar
someArguments = some . argument str . metavar

options = subparser
  (  commandhd "list"         listOptions "List notes"
  <> commandhd "cat"           catOptions "Concatenate notes to STDOUT"
  <> commandhd "edit"         editOptions "Edit notes selected by ID or search terms"
  <> commandhd "tags"         tagsOptions "Display all tags currently in use"
  <> commandhd "check"       checkOptions "Sanity check notes"
  <> commandhd "import"     importOptions "Import a file as a note"
  <> commandhd "new"           newOptions "Create a new note"
  <> commandhd "obsolete" obsoleteOptions "Mark notes as obsolete"
  <> commandhd "rename"     renameOptions "Change title of note"
  ) <|> (None <$> manyArguments "SEARCH TERMS")

listOptions = List
  <$> switch      (lsh "all"  'a' "Include obsoleted notes in search [NOT IMPLEMENTED]")
  <*> switch      (lsh "path" 'p' "List full path of note files")
  <*> strOptional (lsh "exec" 'e' "Pass notes file paths as arguments to COMMAND" <> metavar "COMMAND")
  <*> strOptional (lsh "tag"  't' "Only notes tagged with TAG"                    <> metavar "TAG")
  <*> manyArguments "SEARCH TERMS"
  -- <*> many (argument str $ metavar "SEARCH TERMS")

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

newOptions = New
  <$> switch (lsh "empty" 'e' "Create an empty note")
  <*> argument str (metavar "TAG")
  <*> someArguments "NAME"  -- TODO: should be TITLE?

obsoleteOptions = Obsolete
  <$> switch (long "dry-run" <> help "Show how the files would be renamed, but don't actually do anything")
  <*> strOption (lsh "id" 'i' "The ID of the note to mark as obsolete" <> metavar "ID")

renameOptions = Rename
  <$> switch (long "dry-run" <> help "Show how the files would be renamed, but don't actually do anything")
  <*> strOption (lsh "id" 'i' "The ID of the note to rename" <> metavar "ID")
  <*> someArguments "NAME"

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
