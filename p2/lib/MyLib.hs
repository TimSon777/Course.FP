module MyLib where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad

type Parser = Parsec Void String

cell :: Char -> Parser String
cell delimiter = many (noneOf [delimiter, '\n'])

csvLine :: Char -> Parser [String]
csvLine delimiter = cell delimiter `sepBy` char delimiter

csvFile :: Char -> Bool -> Parser ([String], [[String]])
csvFile delimiter hasHeader = do
    when (delimiter /= ',' && delimiter /= ';') $
        fail "Bad delimiter. Expected `,` or `;`"

    header <- if hasHeader then
                csvLine delimiter <* eol
              else
                return []

    lines <- csvLine delimiter `sepEndBy` eol
    if hasHeader && any ((/= length header) . length) lines then
        fail "Number of cells in row does not match header"
    else
        return (header, lines)

parseCSV :: String -> Char -> Bool -> Either (ParseErrorBundle String Void) ([String], [[String]])
parseCSV input delimiter hasHeader = runParser (csvFile delimiter hasHeader) "" input
