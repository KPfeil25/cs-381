type Prog = [Cmd]

data Cmd
    = LD Integer
    | ADD
    | MULT 
    | DUP
    deriving Show

type Stack = [Integer]

semCmd :: Cmd -> Stack -> Maybe Stack
semCmd (LD i) s = Just (i:s) -- Load the int
semCmd DUP vs@(v:_) = Just (v:vs) -- Take the top one and duplicate it
semCmd ADD (v1:v2:vs) = Just (v1+v2:vs) -- Take the top two, add them
semCmd MULT (v1:v2:vs) = Just (v1*v2:vs) -- Same as add but multiple
semCmd _ _ = Nothing

run :: Prog -> Stack -> Maybe Stack
run []     s = Just(s)
run (c:cs) s = case (semCmd c s) of
                    (Just xs) -> run cs xs
                    (Nothing) -> Nothing

stack1 = [1, 2, 3, 4, 5]
test1 = [LD 3,DUP,ADD,DUP,MULT]
test2 = [LD 3,ADD]
test3 = []
test4 = [ADD, ADD, ADD, ADD]