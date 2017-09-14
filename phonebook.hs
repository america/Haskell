import qualified Data.Map as Map
import Data.Char

phonebook :: Map.Map String String
phonebook = Map.fromList $
  [("betty", "555-2938")
  ,("bonnie", "452-2928")
  ,("pasty", "493-2928")
  ,("lucille", "205-2928")
  ,("wendy", "939-8282")
  ,("penny", "853-2492")]

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit
