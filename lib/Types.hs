module Types where

-- | Command line arguments
data Options = Options
  { optIndent :: !Indent,
    optInput :: !InputMode,
    optDebug :: !Bool
  }
  deriving (Eq, Show)

data InputMode
  = FileInput !FilePath
  | StdIn
  deriving (Eq, Show)

newtype Indent = Indent {unIndent :: Int}
  deriving (Eq, Show)
