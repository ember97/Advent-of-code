import Data.Map as M (Map, insert, lookup, empty, elems)
import Data.Maybe (fromJust)
import Data.List (groupBy)
import Data.Char (isDigit)

type StackMap = Map Integer Stack
type Stack = String


main :: IO()
main = do
    f <- readFile "Day5.txt"
    let infoList = lines f
    let instructions =  ["QFLSR", "TPGQZN", "BQMS", "QBCHJZGT", "SFNBMHP", "GVLSNQCP", "FCW", "MPVWZGHQ", "RNCLDZG"]
    let m = initStackMap instructions
    let result = run 1 (map getInfo infoList) m
    let crate = getCrate result
    print ("crates " ++  show crate)

    let result2 = run 2 (map getInfo infoList) m
    let crate2 = getCrate result2
    print ("crates part 2 " ++ show crate2)


-- Derive all necessary information from a row
-- getInfo "move 1 from 2 to 1" -> [1,2,1]
getInfo :: String -> [Integer]
getInfo = map (toInteger' . tail) . filter (\c -> length c > 1) . groupBy (\_ c -> isDigit c) 

toInteger' :: String -> Integer
toInteger' str = read str :: Integer

initStackMap :: [String] -> StackMap
initStackMap list = go 1 list M.empty
   where go :: Integer -> [String] -> StackMap -> StackMap
         go _ [] m = m
         go n (s:ss) m = go (n+1) ss (M.insert n s m)



-- Part 1 --
run :: Integer -> [[Integer]] -> StackMap -> StackMap
run _ [] m = m
run i ([n,i1,i2]:w) m = run i w (iter i n i1 i2 m)


-- Perform an update
iter :: Integer -> Integer -> Integer -> Integer -> StackMap -> StackMap
iter i n i1 i2 m = M.insert i1 s1' $ M.insert i2 s2' m
   where (s1',s2') = moveFun n (fromJust $ M.lookup i1 m) (fromJust $ M.lookup i2 m )
         moveFun = if i == 1 then move else move2

-- Pop n times from s1 to s2
move :: Integer -> Stack -> Stack -> (Stack, Stack)
move 0 s1 s2 = (s1,s2)
move n s1 s2 = move (n-1) s1' s2'
  where (e, s1') = pop s1
        s2' = push s2 e

-- Used when finished to get top of stacks
getCrate :: StackMap -> String
getCrate m = let values = elems m in go values []
    where go :: [String] -> String -> String
          go [] str = str
          go (s:ss) str = go ss (str ++ [peek s])

-- Stack functions --
pop :: [a] -> (a, [a])
pop [] = error "empty stack"
pop stack = (head stack, tail stack)

push :: [a] -> a -> [a]
push stack e = e:stack

peek :: [a] -> a
peek [] = error "empty stack"
peek stack = head stack




-- Part 2 --
move2 :: Integer -> Stack -> Stack -> (Stack, Stack)
move2 n s1 s2 = (s1', s2')
  where (values, s1') = splitAt (fromIntegral n) s1
        s2' = values ++ s2