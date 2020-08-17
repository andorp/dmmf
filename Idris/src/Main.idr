module Main -- 'where'

import Common.Prelude
import Category.Definition
import Category.Type

main : IO ()
main = do
  let test1 : Result Int Int = Ok 1
  let test2 : Result Int Int = Error 1
  putStrLn $ show test1 -- Ok 1
  putStrLn $ show test2 -- Error 1
  putStrLn $ show $ map @{ResultSuccessFunctor} (+1) test1 -- Ok 2
  putStrLn $ show $ map @{ResultFailureFunctor} (+1) test1 -- Ok 1
  putStrLn $ show $ map @{ResultSuccessFunctor} (+1) test2 -- Ok 1
  putStrLn $ show $ map @{ResultFailureFunctor} (+1) test2 -- Error 2
