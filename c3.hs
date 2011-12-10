import List

l1 :: [a] -> Int
l1 (x:xs) = 1 + l1 xs
l1 [] = 0

--ex 2

--ex 3

s1 (x:xs) = x + s1 xs
s1 [] = 0

m1 a = (s1 a)/ fromIntegral(l1 a)

--ex 4 palyndrome
p1 a
  | a == [] = []
  | otherwise = [head a] ++ (p1 (tail a)) ++ [head a]

--ex 5 is palyndrome
ip1 a
  | a == [] = True
  | otherwise = ((r1 a) == a)
    where r1 a
            | a == [] = []
            | otherwise = (r1 (tail a)) ++ [head a]

--ex 6 sort list of lists by length of list
s2 a = sortBy l2 a
  where l2 a b | length a > length b = GT
               | otherwise = LT

--ex 7 intersperse

--i7 :: a -> [[a]] -> [a]      

i7 a ass = i7sub a ass []
  where i7sub b bss prefix
          | (bss == []) = prefix
          | ((length bss) == 1) = (prefix ++ (head bss)) 
          | otherwise = i7sub b (tail bss) (prefix ++ (head bss) ++ [b])

--i7 a ass = i7sub a ass []

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)
                     
--h8 Tree height
h8 :: Tree t -> Int
                     
h8 Empty = 0
h8 (Node a b c) = 1 + (max (h8 b) (h8 c))

--ex 9
data Direction = 
  Left
  | Right
  | Straight
  deriving (Show)

--ex 10
data P2D = P2D {
  x :: Int 
 ,y :: Int
  } deriving (Eq, Show)

f10 p1 p2 p3 
  | vprod p1 p2 p3 == 0 = Straight
  | vprod p1 p2 p3 < 0 = Main.Right
  | otherwise = Main.Left
                
-- P( a, b) = a1 b2 - a2 b1
-- a1 = x2 - x1
-- a2 = x3 - x2

-- b1 = y2 - y1
-- b2 = y3 - y2
vprod (P2D x1 y1) (P2D x2 y2) (P2D x3 y3) = (a1 * b2) - (a2 * b1)  
                                            where a1 = x2 - x1
                                                  a2 = x3 - x2
                                                  b1 = y2 - y1
                                                  b2 = y3 - y2


--ex 11
f11 :: [P2D] -> [Main.Direction]
f11 (x1:(x2:(x3:xs))) = (f10 x1 x2 x3: f11(x2:(x3:xs)))
f11 _ = []

--ex 12
lowerLefter (P2D x1 y1) (P2D x2 y2) | (y1 < y2) || ((y1 == y2) && (x1 < x2)) = LT
                                     | (x1 == x2) && (y1 == y2) = EQ
                                     | otherwise = GT
                                                   
-- counter clockwise
data Dir = 
  CCW --counter clockwise
  | C --collinear
  | CW --clockwise
    deriving (Eq,Show)
             
ccw :: P2D -> P2D -> P2D -> Main.Dir
ccw (P2D x1 y1) (P2D x2 y2) (P2D x3 y3)  
  | d > 0 = CCW
  | d == 0 = C
  | d < 0 = CW
    where d = (x2 - x1) * (y3 - y1) - (y2 - y1) *  (x3 - x1)
                                          
-- p1 lower andgle than p2
lowerAngle :: P2D -> P2D -> P2D -> Ordering
lowerAngle pBase p1 p2 
  | dir == CCW = LT
  | dir == C = EQ
  | dir == CW = GT
    where dir = ccw pBase p1 p2

sortByAngle :: P2D -> [P2D] -> [P2D]                    
sortByAngle pBase a = sortBy (lowerAngle pBase) a

startPoint :: [P2D] -> P2D
startPoint a = head (sortBy lowerLefter a)

orderPs :: [P2D] -> [P2D]
orderPs a = sortBy (lowerAngle s) (s : delete s a)
  where s = startPoint a

gscan :: [P2D] -> [P2D] -> [P2D]
gscan hull [] = hull
gscan [] rest = gscan (take 2 a) (drop 2 a)
  where a = orderPs rest
gscan hull rest 
  | ((cCW == CW) || (cCW == C)) = let
    res | (null (init (init hull))) = gscan (head hull : [head rest]) (tail rest)
        | otherwise                 = gscan (init hull)                   rest
    in res
  | (cCW == CCW) = gscan (hull ++ [head rest]) (tail rest)
    where cCW = ccw hn1 hn r1
            where hn  = last hull
                  hn1 = last (init hull)
                  r1  = head rest

f12 :: [P2D] -> [P2D]
f12 a = gscan [] a