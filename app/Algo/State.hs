{-# LANGUAGE TupleSections #-}

module Algo.State where
    
import Control.Monad


-- newtype State s a = St (s -> (s, a))

-- instance Functor (State s) where
--     fmap g (St f) = St $ \s -> let (s1, a) = f s in (s1, g a)

-- instance Applicative (State s) where
--     (St g) <*> (St f) = St $ \s -> let (s1, a) = f s; (s2, h) = g s1 in (s2, h a)
--     pure a = St $ \s -> (s, a)

-- instance Monad (State s) where
--     (St f) >>= g = St $ \s -> let (s1, a) = f s; (St h) = g a in h s1

-- runState :: s -> State s a -> (s, a)
-- runState s (State f) = f s



-- filterSum :: String -> (String, Int)
-- filterSum xs = go xs ("", 0)
--     where 
--         go [] (result, state) = (result, state)
--         go (x:xs) (result, state)
--             | x >= 'a' && x <= 'z' = go xs (x:result, state)
--             | otherwise = go xs (result, (read [x] :: Int) + state)

-- -- ghci> filterSum "a1b2c3"
-- -- ("cba",6)

-- go :: String -> String -> Int -> (String, Int)
-- go [] result state = (result, state)
-- go (x:xs) result state
--     | x >= 'a' && x <= 'z' = go xs (x:result) state
--     | otherwise = go xs result ((read [x] :: Int) + state)

-- go1 :: String -> String -> Int -> (String, Int)
-- go1 [] result = \state -> (result, state)
-- go1 (x:xs) result =
--     if x >= 'a' && x <= 'z' 
--         then \state -> go1 xs (x:result) state
--         else \state -> go1 xs result ((read [x] :: Int) + state)

-- stateFn resultFn = \state -> (resultFn result, state)
-- stateModify result modi = \state -> (result, modi state)

-- go2 :: String -> String -> Int -> (String, Int)
-- go2 [] result = stateFn result
-- go2 (x:xs) result = 
--     if x >= 'a' && x <= 'z' 
--         then \state -> let (r, s) = go2 xs (x:result) state in stateFn r
--         else stateModify (go2 xs result) (\state -> (read [x] :: Int) + state)



-- filterSum1 xs = foldl go ("", 0) xs
--     where 
--         go (result, state) x
--             | x >= 'a' && x <= 'z' = (x:result, state)
--             | otherwise = (result, (read [x] :: Int) + state)

-- ghci> filterSum1 "a1b2c3"
-- ("cba",6)

--  :: (s, a) -> (s, b)

-- newtype State s a = State { runState :: s -> (a, s) }

-- instance Functor (State s) where
--     fmap = liftM

-- instance Applicative (State s) where
--     pure a = State $ (a, )
--     (<*>) = ap

-- instance Monad (State s) where
--     return = pure
--     (State f) >>= g = State $ \s -> let (a, s') = f s; State h = g a in h s'

-- putState :: s -> State s ()
-- putState s = State $ \_ -> ((), s)

-- getState :: State s s
-- getState = State $ \s1 -> (s1, s1)

data Direction = Forward | Backward deriving (Eq, Show)
type Position = Int

walk :: (Position, Direction) -> (Position, Direction)
walk (9,    Forward)    = (10,       Backward)
walk (pos,  Forward)    = (pos + 1, Forward)
walk (1,    Backward)   = (0,    Forward)
walk (pos,  Backward)   = (pos - 1, Backward)

-- turn Forward = Backward
-- turn Backward = Forward

-- step Forward pos = pos + 1
-- step Backward pos = pos - 1

-- walks :: Position -> State Direction Position
-- walks pos = do
--     dir <- getState
--     let newDir = if pos == 0 then turn dir else dir
--     putState newDir
--     return $ if newDir == Forward then pos + 1 else pos - 1


walk4 :: (Position, Direction) -> (Position, Direction)
walk4 (pos, dir) =
    let (pos1, dir1) = walk (pos, dir)
        -- some other stuff
        (pos2, dir2) = walk (pos1, dir1)
        -- some other stuff
        (pos3, dir3) = walk (pos2, dir2)
        -- some other stuff
    in walk (pos3, dir3)

-- newtype WalkState = WS (Direction -> (Position, Direction))

walks :: Position -> State Direction Position
walks pos = State $ \dir -> case (pos, dir) of
                            (9,    Forward)    -> (10,       Backward)
                            (pos,  Forward)    -> (pos + 1, Forward)
                            (1,    Backward)   -> (0,    Forward)
                            (pos,  Backward)   -> (pos - 1, Backward)

-- bind :: WalkState -> WalkState

-- bind :: State s a -> (a -> State s b) -> State s b
-- bind (State f) g = State $ \s -> f s

-- f :: s -> (s, a)
-- g :: (a -> s -> (s, b)) -> (s, a) -> (s, b)
-- g = uncurry . flip

-- -- a stateful function is (a -> b) with state s on the side
-- (a, s) -> (b, s)
-- -- composition 
-- -- (a -> b) -> (b -> c) -> (a -> c)
--    ((a, s) -> (b, s)) -> ((b, s) -> (c, s)) -> ((a, s) -> (c, s))
-- == (a -> s -> (b, s)) -> (b -> s -> (c, s)) -> (a -> s -> (c, s))        -- currying
-- == (a -> (s -> (b, s))) -> (b -> (s -> (c, s))) -> (a -> (s -> (c, s)))  -- right associativity
--            let State s a = State (s -> (a, s))
-- == (a -> State s b)    ->  (b -> State s c)     -> (a -> State s c)
-- == (>>>)


charToInt :: Char -> Int
charToInt c = read [c]

intToList :: Int -> [Int]
intToList n = [n]

chainPlain :: (Char -> Int) -> (Int -> [Int]) -> (Char -> [Int])
chainPlain = flip (.)

charToIntList :: Char -> [Int]
charToIntList = chainPlain charToInt intToList

type EvilState = Int

charToIntS :: (Char, EvilState) -> (Int, EvilState)
charToIntS (c, s) = let s1 = read [c] in (s1 * s, s1)

intToListS :: (Int, EvilState) -> ([Int], EvilState)
intToListS (n, s) = let s1 = n * s in ([s1], s1)

charToIntListS :: (Char, EvilState) -> ([Int], EvilState)
charToIntListS = intToListS . charToIntS

newtype State s a = State (s -> (a, s))

instance Functor (State s) where
    fmap a2b (State f) = State $ \s -> let (a, s1) = f s in (a2b a, s1)

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    (State g) <*> (State f) = State $ \s -> let (a, s1) = f s; (a2b, s2) = g s1 in (a2b a, s2)

instance Monad (State s) where
    return = pure
    (State f) >>= g = State $ \s -> let (a, s1) = f s; (State h) = g a in h s1

charToIntM :: Char -> State EvilState Int
charToIntM c = State $ \s -> let s1 = read [c] in (s1 * s, s1)

intToListM :: Int -> State EvilState [Int]
intToListM n = State $ \s -> let s1 = n * s in ([s1], s1)

charToIntListM :: Char -> State EvilState [Int]
charToIntListM c = do
    n <- charToIntM c
    ns <- intToListM n
    return ns

getState :: State s s
getState = State $ \s -> (s, s)

putState :: s -> State s ()
putState s = State $ \_ -> ((), s) 

charToIntM1 :: Char -> State EvilState Int
charToIntM1 c = do
    s <- getState
    let s1 = read [c]
    putState s1
    return $ s1 * s

intToListM1 :: Int -> State EvilState [Int]
intToListM1 n = do
    s <- getState
    let s1 = n * s
    putState s1
    return [s1]

charToIntListM1 :: Char -> State EvilState [Int]
charToIntListM1 c = do
    n <- charToIntM1 c
    ns <- intToListM1 n
    return ns

charToIntListM' c = do
    let s1 = read [c]
    putState s1

mapSum :: Num a => (a -> b) -> a -> State a b
mapSum f n = do
    currentSum <- getState
    putState (currentSum + n)
    return $ f n

mapSum2 f xs = mapM (\x -> do { tot <- getState; putState (tot + x); return (f x); }) xs

mapSum1 :: Num a => (a -> b) -> [a] -> (a, [b])
mapSum1 f xs = foldr (\x (tot, ys) -> (tot + x, f x : ys)) (0, []) xs

modifyState :: (s -> s) -> State s ()
modifyState f = do
    s <- getState
    putState (f s)

mapSum3 f xs = mapM (\x -> do { modifyState (+ x); return (f x); }) xs

greetings = [ "Hello", "Howdy", "Hi", "G'day" ]
index = 0

type Name = String
type Greeting = (String, Name)

greet :: Name -> Greeting
greet name = (greetings !! index, name)

type GState = Int

greetS :: (Name, GState) -> (Greeting, GState)
greetS (name, st) = ((greetings !! st, name), st + 1)

type DoubleGreeting = (String, Name, String)

doubleGreetS :: (Greeting, GState) -> (DoubleGreeting, GState)
doubleGreetS ((greeting, name), st) = ((greeting, name, greetings !! st), st + 1)

-- composeS 
--     :: ((Name, GState) -> (Greeting, GState)) 
--     -> ((Greeting, GState) -> (DoubleGreeting, GState)) 
--     -> ((Name, GState) -> (DoubleGreeting, GState))
-- composeS f1 f2 = f2 . f1
composeS f1 f2 = 
    \(a, st) -> 
        let (b, st1) = f1 (a, st) 
        in f2 (b, st1)

highGreet :: (Name, GState) -> (DoubleGreeting, GState)
highGreet = composeS greetS doubleGreetS

greetM :: Name -> State GState Greeting
greetM name = State $ \st -> ((greetings !! st, name), st + 1)

doubleGreetM :: Greeting -> State GState DoubleGreeting
doubleGreetM (greeting, name) = State $ \st -> ((greeting, name, greetings !! st), st + 1)

highGreetM = greetM >=> doubleGreetM

greetM' :: Name -> State GState Greeting
greetM' name = do
    st <- getState
    putState (st + 1)
    return (greetings !! st, name)

doubleGreetM' :: Greeting -> State GState DoubleGreeting
doubleGreetM' (greeting, name) = do
    st <- getState
    putState (st + 1)
    return (greeting, name, greetings !! st)

highGreetM' = greetM' >=> doubleGreetM'


-- type Error = String

-- getData :: Int -> Either DataError Int
-- getData n = Left "Fails"

-- serviceFn :: (Int, Int) -> Either ServiceError Int
-- serviceFn (_, id) = do
--     res = getData id
--     setData (res + 1)
    
-- presenter :: Int -> HttpResponse
-- presenter req =
--     case serviceFn req of
--         Left err -> setStatus 400 (err.ToJSON())
--         Right res -> jsonify res