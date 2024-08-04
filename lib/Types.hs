module Types where

-- | Command line arguments
data Options = Options
  { optSpacing :: !Int,
    optInput :: InputMode,
    optDebug :: !Bool
  }
  deriving (Eq, Show)

data InputMode
  = FileInput !FilePath
  | StdIn
  deriving (Eq, Show)
