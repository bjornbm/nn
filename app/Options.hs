{-# LANGUAGE DisambiguateRecordFields #-}

{-

Global options:

    --home NN_HOME
    --editor NN_EDITOR

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

Note creation commands:

    nn new (-i ID) TAG NAME
    nn save (-i ID) -t TAG (--name NAME) FILE

Sanity checking commands:

    nn check
    nn stats

-}


-- TODO creat Types module?


module Options where


import Options.Applicative
import Data.Semigroup ((<>))
--import Prelude hiding (all)


data Options = Options
  { nnHome     :: FilePath
  -- TODO other global options
  , optCommand :: Command
  }


data Command
  = List  { all :: Bool, exec :: Maybe String, tagged :: Maybe String, terms :: [String] }
  | Cat   { noheaders :: Bool, id :: String }
  | Edit  { id :: String }
  | Tags  { popularity :: Bool }
  | Check { names :: Bool, references :: Bool }
  | Save  { rename :: Maybe String, tag :: String, file :: String }
  | New   { empty :: Bool, tag :: String, name :: [String] }
  | None  { terms :: [String] }
  deriving (Show) -- , Data, Typeable)



infoh parser = info (helper <*> parser)
commandh name parser = command name . infoh parser
commandhd name parser = command name . infoh parser . progDesc
strOptional = optional . strOption
lsh l s h = long l <> short s <> help h
manyArguments = many . argument str . metavar
someArguments = some . argument str . metavar

options = subparser
  (  commandhd "list"   listOptions "List notes"
  <> commandhd "cat"     catOptions "Concatenate notes to STDOUT"
  <> commandhd "edit"   editOptions "Edit notes"
  <> commandhd "tags"   tagsOptions "Display all tags currently in use"
  <> commandhd "check" checkOptions "Sanity check notes"
  <> commandhd "save"   saveOptions "Import any file as a note"
  <> commandhd "new"     newOptions "Create a new note"
  ) <|> (None <$> manyArguments "SEARCH TERMS")

listOptions = List
  <$> switch      (lsh "all"  'a' "Include obsoleted notes in search")
  <*> strOptional (lsh "exec" 'e' "Pass notes file paths as arguments to COMMAND" <> metavar "COMMAND")
  <*> strOptional (lsh "tag"  't' "Only notes tagged with TAG"                    <> metavar "TAG")
  <*> manyArguments "SEARCH TERMS"
  -- <*> many (argument str $ metavar "SEARCH TERMS")

catOptions :: Parser Command
catOptions = Cat
  <$> switch       (lsh "noheaders" 'n' "Do not include headers in output")
  <*> argument str (help "The ID of the note to cat" <> metavar "ID")

editOptions = Edit
  <$> strOption (lsh "id" 'i' "The ID of the note to cat" <> metavar "ID")

tagsOptions = Tags
  <$> switch (lsh "popularity" 'p' "Show and sort tags by popularity")

checkOptions = Check
  <$> switch (lsh "names"      'n' "List badly named notes")
  <*> switch (lsh "references" 'r' "List notes containing bad note references")

saveOptions = Save
  <$> strOptional  (lsh "rename" 'r' "Save with descriptive name NAME" <> metavar "NAME")  -- TODO: should be TITLE?
  <*> argument str (metavar "TAG")
  <*> argument str (metavar "FILE")

newOptions = New
  <$> switch (lsh "empty" 'e' "Create an empty note")
  <*> argument str (metavar "TAG")
  <*> someArguments "NAME"  -- TODO: should be TITLE?


descText = "nn is a tool for conveniently and efficiently creating, searching, and displaying notes."
        <> "Different behaviours are invoked by different subcommands."
        <> "The default behaviour, when no subcommand is passed, is to search for notes."

description = fullDesc
    <> header "nn - a note management tool"
    <> progDesc descText
    <> footer "(c) 2016, Bjorn Buckwalter"

parseCommand :: IO Command
parseCommand = execParser (info (helper <*> options) description)
