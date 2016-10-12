import Test.DocTest

main :: IO ()
main = do putStrLn ""; doctest ["src/"]
