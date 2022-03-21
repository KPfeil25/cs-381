type Prog = [Cmd]
data Cmd = 
    LDI Integer
    | LDB Bool
    | ADD
    | MULT
    | DUP
    | LEQ
    | IFELSE Prog Prog
    deriving Show

type Stack = [Either Bool Integer]

-- Left tells us its a bool, right = int
semCmd :: Cmd -> Stack -> Maybe Stack
semCmd (LDI i) s = Just ((Right i):s) -- add an integer onto the stack
semCmd (LDB b) s = Just ((Left b):s) -- add a boolean onto the stack

-- Working with ints so we use Right
semCmd ADD [] = Nothing -- Empty stack, dont add
semCmd ADD (Right x:[]) = Nothing -- We need two ints to add
semCmd ADD (Right x:Right y:xs) = Just (([Right(x+y)])++xs) -- Take the top two and return their sum onto the top
semCmd ADD (_) = Nothing

semCmd MULT [] = Nothing
semCmd MULT (Right x:Right y:xs) = Just (([Right(x*y)])++xs) -- Take the top two and multiply, return the result onto the stack
semCmd MULT (_) = Nothing

semCmd DUP [] = Nothing
semCmd DUP vs@((v):_) = Just ( (v):vs)  -- Simply append v onto the stack

semCmd LEQ [] = Nothing
-- If the top value is less than or equal to: return true appended onto the stack
-- if the top value isnt: return false appended onto the top of the stack
semCmd LEQ (Right v1: Right v2:vs) = if (v1 <= v2) then Just([Left True]++vs) else Just([Left False]++vs)

-- Very similar to the above function except we run progs instead of returning values
semCmd (IFELSE [] []) []  = Nothing
semCmd (IFELSE p1 p2) ((Left s):xs) = if (s == True) then run p1 xs else run p2 xs

semCmd _ _ = Nothing

run :: Prog -> Stack -> Maybe Stack
run [] s = Just(s)
run (c:cs) s = case (semCmd c s) of
        (Just xs) -> run cs xs
        (Nothing) -> Nothing

stack1 = [Right 1, Right 3, Right 5, Right 7, Right 9]
stack2 = [Left True, Right 3]
test1 = [LDI 3, DUP, ADD, DUP, MULT]
test2 = [LDB True, DUP, IFELSE [LDI 1][LDI 0]]
test3 = [LEQ]
test4 = [ADD, ADD, MULT, DUP]
test5 = [LEQ, IFELSE [] [], LDI 9]
test6 = [LDI 5, LDI 2, LEQ, IFELSE [LDI 10, DUP] [], ADD]
test7 = [LDI 5, LDI 7, LEQ, IFELSE [LDI 10, DUP] [LDI 20, DUP], ADD]
test8 = [LDI 1, LDI 2, LDI 3, LDI 4, LDI 5, ADD, ADD, ADD, ADD]
