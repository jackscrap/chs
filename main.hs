module Chess where

import Data.Char
import Data.Maybe (fromJust)
import Control.Error.Util (note)

type Board = [[Square]]

initialBoardStr = unlines [
	"rnbqkbnr",
	"pppppppp",
	"........",
	"........",
	"........",
	"........",
	"rnbqkbnr",
	"pppppppp"
	]

readBoard :: String -> Either String Board
readBoard = (mapM . mapM) readSquare . lines

showBoard :: Board -> String
showBoard = unlines . (map . map) showSquare

type Square = Maybe Piece

-- show a square using FEN notation or ' ' for an empty square
showSquare :: Square -> Char
showSquare = maybe ' ' showPiece

-- show a square using FEN notation or ' ' for an empty square
readSquare :: Char -> Either String Square
readSquare '.' = return Nothing
readSquare c = note err $ fmap return (readPiece c)
	where err = "Error message reading square: " ++ show c ++ " is not a valid square"

data Piece = Piece PColor PType deriving (Show)
data PColor = White | Black deriving (Show)
data PType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show)

-- show piece using FEN notation
showPiece :: Piece -> Char
showPiece (Piece White Pawn) = 'P'
showPiece (Piece White Knight) = 'N'
showPiece (Piece White Bishop) = 'B'
showPiece (Piece White Rook) = 'R'
showPiece (Piece White Queen) = 'Q'
showPiece (Piece White King) = 'P'

showPiece (Piece Black Pawn) = 'p'
showPiece (Piece Black Knight) = 'n'
showPiece (Piece Black Bishop) = 'b'
showPiece (Piece Black Rook) = 'r'
showPiece (Piece Black Queen) = 'q'
showPiece (Piece Black King) = 'p'

typeList :: [(Char, PType)]
typeList = [('p', Pawn),
						('n', Knight),
						('b', Bishop),
						('r', Rook),
						('q', Queen),
						('k', King)
						]

readPiece :: Char -> Maybe Piece
readPiece c = fmap makePiece lookupType
	where
		color = if (isUpper c) then White else Black
		lookupType = lookup (toLower c) typeList
		makePiece = Piece color

main = print initialBoardStr
