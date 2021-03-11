import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List

-- use maps instead of lists for easy checking of membership

-- anamorphism 
divideGroups :: (a -> Bool) -> [a] -> [a] -> [[a]] -> [[a]]
divideGroups divide [] group total = total ++ [group]
divideGroups divide (x:xs) group total = 
  if divide x 
  then divideGroups divide xs [] total ++ [group]
  else divideGroups divide xs (group ++ [x]) total 

-- check what kind of morphism is this
processGroup :: Ord a => [a] -> Map a Bool -> Map a Bool 
processGroup [] proc = proc
processGroup (x:xs) proc = 
  if Map.member x proc 
  then processGroup xs proc  
  else processGroup xs (Map.insert x True proc) 

-- maybe this counts as an hylomorphism
processAllGroups :: Ord a => [[a]] -> Map a Bool -> Int
processAllGroups [] acc = length (Map.keys acc)
processAllGroups (x:xs) acc = 
  let next = processGroup x acc in 
    processAllGroups xs next

processAllGroupsFree :: Ord a => [[a]] -> Int
processAllGroupsFree gp = processAllGroups gp Map.empty 

isEmpty :: Text.Text -> Bool
isEmpty x = x == Text.empty

main = do
  ls <- fmap Text.lines (Text.readFile "input")
  let groups = divideGroups isEmpty ls [] []
  let mapped = map (\x -> (map Text.unpack x)) groups
  let countd = map processAllGroupsFree mapped
  -- foldr is the definition of catamorphism
  print (foldr (+) 0 z)

-- output: 6310 gg
