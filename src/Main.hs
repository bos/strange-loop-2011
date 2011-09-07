import System.Environment

main = do
  args <- getArgs
  putStrLn ("So! Your args are " ++ show args)
