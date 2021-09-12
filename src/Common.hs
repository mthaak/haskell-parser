module Common
  ( Coordinates,
    Error (..),
    errorLoc,
  )
where

type Row = Int

type Col = Int

type Coordinates = (Row, Col)

data Error
  = ScanError Coordinates String
  | LayoutError Coordinates String
  | ParseError Coordinates String
  deriving (Show, Eq)

errorLoc :: Error -> Coordinates
errorLoc (ScanError c _) = c
errorLoc (LayoutError c _) = c
errorLoc (ParseError c _) = c

errorMsg :: Error -> String
errorMsg (ScanError _ s) = s
errorMsg (LayoutError _ s) = s
errorMsg (ParseError _ s) = s
