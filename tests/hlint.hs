import           Language.Haskell.HLint3
import           System.Exit

main :: IO ()
main = do
  ideas <- hlint ["app", "src", "tests", "--find", "tests/hlint-custom.hs"]
  if null ideas
    then exitSuccess
    else exitFailure
