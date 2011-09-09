import System.Environment
import Download
import Links

main = do
  args <- getArgs
  putStrLn ("So! Your args are " ++ show args)
  page <- download (head args)
  print (links page)
