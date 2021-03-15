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
processWord :: Ord a => [a] -> Map a Bool -> Map a Bool 
processWord [] proc = proc
processWord (x:xs) proc = 
  if Map.member x proc 
  then processWord xs proc  
  else processWord xs (Map.insert x True proc) 

-- maybe this counts as an hylomorphism
processGroup :: Ord a => [[a]] -> Map a Bool -> Int
processGroup [] acc = length (Map.keys acc)
processGroup (x:xs) acc = 
  let next = processWord x acc in 
    processGroup xs next

processGroupConjunction :: Ord a => [[a]] -> Map a Bool -> [a] 
processGroupConjunction [] acc = Map.keys acc
processGroupConjunction (x:xs) acc = 
  let next = processWord x acc in 
    processGroupConjunction xs next

contains :: Ord a => a -> [a] -> Bool
contains a = (Map.member a) . (\x -> processWord x Map.empty)

allContains :: Ord a => [[a]] -> a -> Bool
allContains [] _ = True
allContains (x:xs) value = 
  if contains value x
  then allContains xs value
  else False

-- the free conjunction is the letter b of challenge 6
processGroupFreeConjunction :: Ord a => [[a]] -> [a] 
processGroupFreeConjunction gp = 
  let leters = processGroupConjunction gp Map.empty in
    filter (allContains gp) leters

processGroupFree :: Ord a => [[a]] -> Int
processGroupFree gp = processGroup gp Map.empty 

isEmpty :: Text.Text -> Bool
isEmpty x = x == Text.empty

main = do
  ls <- fmap Text.lines (Text.readFile "input")
  let groups = divideGroups isEmpty ls [] []
  let mapped = map (\x -> (map Text.unpack x)) groups
  let countd = map (length . processGroupFreeConjunction) mapped
  -- foldr is the definition of catamorphism
  print (foldr (+) 0 countd)

-- output: 6310 and 3193 gg
