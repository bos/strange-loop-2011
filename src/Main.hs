import System.Environment
import Download

main = do
  args <- getArgs
  putStrLn ("So! Your args are " ++ show args)
  page <- download (head args)
  print page
