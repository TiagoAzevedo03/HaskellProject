-- PFL 2023/24 - Haskell practical assignment quickstart
import Data.List (sortOn, intercalate, lookup)
import Data.List (sort)
import Data.Ord (comparing)
import Data.Char (isDigit, isSpace, digitToInt, isLower, isAlpha)
import Debug.Trace

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data Value = 
  IntValue Integer | BoolValue Bool 
  deriving Show

-- Type to represent the machine's stack
type Stack = [Value]

-- Type to represent the machine's state
type State = [(String, Value)]

createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = []

stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [x] = valueToStr x
stack2Str (x:xs) = valueToStr x ++ "," ++ stack2Str xs

valueToStr :: Value -> String
valueToStr (IntValue intValue) = show intValue
valueToStr (BoolValue boolValue) = show boolValue

state2Str :: State -> String
state2Str [] = ""
state2Str [(x, y)] = x ++ "=" ++ valueToStr y
state2Str ((x, y):xs) = x ++ "=" ++ valueToStr y ++ "," ++ state2Str xs

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) =  ([], stack, state)
run ((i: rc), sk, st) = run(rc, sk1, st1)
                        where (sk1, st1) = runInst (i, sk, st)

runInst :: (Inst, Stack, State) -> (Stack, State)
runInst (Push value, sk, st) = ([IntValue value] ++ sk, st)
runInst (Tru, sk, st) = ([BoolValue True] ++ sk, st)
runInst (Fals, sk, st) = ([BoolValue False] ++ sk, st)
runInst (Store _, [], _) =  error "Run-time error"
runInst (Store str, sk, st) = (tail sk, sortOn fst ([(str, head sk)] ++ filter (\x -> str /= (fst x)) st))
runInst (Fetch str, sk, st) = case lookup str st of 
  Nothing -> error "Run-time error"
  Just x -> ([x] ++ sk, st)
runInst (Neg, sk, st) = 
  case sk of 
    BoolValue x : restSk -> ([BoolValue (not x)] ++ restSk, st)
    _ -> error "Run-time error"
runInst (Branch c1 c2, sk, st) =
  case sk of 
    BoolValue x : restSk -> (a, b) where (_,a,b) = run (if x then c1 else c2, restSk, st)
    _ -> error "Run-time error" 
runInst (Loop c1 c2, sk, st) = 
  (a, b) where (_, a, b) = run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]], sk, st)
runInst (Noop, sk, st) = (sk, st)
runInst (And, sk, st) =
  case sk of
    BoolValue x : BoolValue y : restSk ->  ([BoolValue (x && y)] ++ restSk, st)
    _ -> error "Run-time error"
runInst (Add, sk, st) =
  case sk of
    IntValue x : IntValue y : restSk ->  ([IntValue (x + y)] ++ restSk, st)
    _ -> error "Run-time error"
runInst (Sub, sk, st) =
  case sk of
    IntValue x : IntValue y : restSk ->  ([IntValue (x - y)] ++ restSk, st)
    _ -> error "Run-time error"
runInst (Mult, sk, st) =
  case sk of
    IntValue x : IntValue y : restSk ->  ([IntValue (x * y)] ++ restSk, st)
    _ -> error "Run-time error"
runInst (Equ, sk, st) =
  case sk of
    BoolValue x : BoolValue y : restSk ->  ([BoolValue (x == y)] ++ restSk, st)
    IntValue x : IntValue y : restSk -> ([BoolValue(x == y)] ++ restSk, st)
    _ -> error "Run-time error"
runInst (Le, sk, st) =
  case sk of
    IntValue x : IntValue y : restSk ->  ([BoolValue (x <= y)] ++ restSk, st)
    _ -> error "Run-time error"

testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

data Aexp = IntValue' Integer | Variable' String | Add' Aexp Aexp | Sub' Aexp Aexp | Mul' Aexp Aexp
  deriving Show

data Bexp = BoolValue' Bool | Eqb' Bexp Bexp | And' Bexp Bexp | Not' Bexp | Le' Aexp Aexp | Eqa' Aexp Aexp
  deriving Show

data Stm = While Bexp [Stm] | Attrib String Aexp | If Bexp [Stm] [Stm] | Aexp' Aexp
  deriving Show

type Program = [Stm]

compA :: Aexp -> Code
compA (IntValue' x) = [Push x]
compA (Variable' variable) = [Fetch variable]
compA (Add' a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (Sub' a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (Mul' a1 a2) = compA a2 ++ compA a1 ++ [Mult]   

compB :: Bexp -> Code
compB (BoolValue' b) | b = [Tru]
                     | otherwise = [Fals]
compB (Eqb' b1 b2) = compB b2 ++ compB b1 ++ [Equ]
compB (And' b1 b2) = compB b2 ++ compB b1 ++ [And]
compB (Eqa' a1 a2) = compA a2 ++ compA a1 ++ [Equ]
compB (Le' a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (Not' b) = compB b ++ [Neg]

compStm :: Stm -> Code
compStm (Aexp' x) = compA x
compStm (Attrib str exp) = compA exp ++ [Store str]
compStm (If expB s1 s2) = compB expB ++ [Branch (compile s1) (compile s2)]
compStm (While expB s1) = [Loop (compB expB) (compile s1)]

compile :: Program -> Code
compile [] = []
compile (x : xs) = compStm x ++ compile xs

data Token = IntTok Integer | VarTok String | AddTok | SubTok | MulTok 
  | OpenTok | CloseTok | BoolTok Bool | EqaTok | AndTok | NotTok | LeTok
  | WhileTok | AttribTok | IfTok | ThenTok | ElseTok | EqbTok | DoTok
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer ('+' : restStr) = AddTok : lexer restStr
lexer ('-' : restStr) = SubTok : lexer restStr
lexer ('*' : restStr) = MulTok : lexer restStr
lexer ('(' : restStr) = OpenTok : lexer restStr
lexer (')' : restStr) = CloseTok : lexer restStr
lexer ('=' : restStr) | head restStr == '=' = EqaTok : lexer (drop 1 restStr)
                      | otherwise = EqbTok : lexer restStr
lexer ('a' : restStr) | take 2 restStr == "nd" = AndTok : lexer (drop 2 restStr)
lexer ('n' : restStr) | take 2 restStr == "ot" = NotTok : lexer (drop 2 restStr)
lexer ('<' : restStr) | head restStr == '=' = LeTok : lexer (tail restStr)
lexer (':' : restStr) | head restStr == '=' = AttribTok : lexer (tail restStr)
lexer (chr : restStr) | isSpace chr = lexer restStr
lexer (';' : restStr) = lexer restStr
lexer ('i' : restStr) | head restStr == 'f' = IfTok : lexer (tail restStr)
lexer ('t' : restStr) | take 3 restStr == "hen" = ThenTok : lexer (drop 3 restStr)
lexer ('e' : restStr) | take 3 restStr == "lse" = ElseTok : lexer (drop 3 restStr)
lexer ('T' : restStr) | take 3 restStr == "rue" = BoolTok True : lexer (drop 3 restStr)
lexer ('F' : restStr) | take 4 restStr == "alse" = BoolTok False : lexer (drop 4 restStr)
lexer ('w' : restStr) | take 4 restStr == "hile" = WhileTok : lexer (drop 4 restStr)
lexer ('d' : restStr) | head restStr == 'o' = DoTok : lexer (tail restStr)
lexer str@(chr : _) 
  | isDigit chr = IntTok (stringToInt digitStr) : lexer restDigit
  | isLower chr = VarTok (stringToVar varStr) : lexer restVar
    where 
        (digitStr, restDigit) = break (not . isDigit) str
        stringToInt :: String -> Integer
        stringToInt=foldl (\acc chr->10*acc+ toInteger (digitToInt chr)) 0

        (varStr, restVar) = break (not . isAlpha) str
        stringToVar :: String -> String
        stringToVar = foldl (\acc chr->acc ++ [chr]) ""
lexer (_ : restString) = error ("Run-time error: Unexpected character")

parseAexp :: [Token] -> Maybe(Aexp, [Token])
parseAexp tokens = 
  case parseTerm tokens of 
    Just(re, moreTokens) -> extendAexp (re, moreTokens)
    _ -> Nothing
extendAexp :: (Aexp, [Token]) -> Maybe(Aexp, [Token])
extendAexp (e1, op : afterAdd) | elem op [AddTok, SubTok]=
  case parseTerm afterAdd of
    Just (e2, moreTokens) -> extendAexp ((if op == AddTok then Add' else Sub') e1 e2, moreTokens)
    Nothing -> Just (e1, afterAdd)
extendAexp (e1, t : restTokens) = Just(e1, t : restTokens)
extendAexp (e1, []) = Just(e1, [])

parseTerm :: [Token] -> Maybe(Aexp, [Token])
parseTerm tokens = 
  case parseFactor tokens of 
    Just(re, moreTokens) -> extendTerm (re, moreTokens)
    _ -> Nothing
extendTerm :: (Aexp, [Token]) -> Maybe(Aexp, [Token])
extendTerm (e1, MulTok: afterMul) = 
  case parseFactor afterMul of
    Just (e2, moreTokens) -> extendTerm (Mul' e1 e2, moreTokens)
    Nothing -> Just (e1, afterMul)
extendTerm (e1, t : restTokens) = Just(e1, t : restTokens)
extendTerm (e1, []) = Just(e1, [])

parseFactor :: [Token] -> Maybe(Aexp, [Token])
parseFactor (OpenTok : restTokens) =
  case parseAexp restTokens of 
    Just(re, CloseTok : moreTokens) -> Just (re, moreTokens)
    _ -> Nothing
parseFactor (IntTok n : restTokens) = Just(IntValue' n, restTokens)
parseFactor (VarTok v : restTokens) = Just(Variable' v, restTokens)
parseFactor tokens = error "Run-time error"

parseBool :: [Token] -> Maybe(Bexp, [Token])
parseBool (OpenTok : restTokens) =
  case parseBexp restTokens of 
    Just(re, CloseTok : moreTokens) -> Just (re, moreTokens)
    _ -> Nothing
parseBool (BoolTok n : restTokens) = Just(BoolValue' n, restTokens)
parseBool (NotTok : restTokens) = 
  case parseBool restTokens of
    Just (re, moreTokens) -> Just (Not' re, moreTokens)
    _ -> Nothing
parseBool tokens = 
  case parseAexp tokens of 
    Just(exp1, EqaTok : moreTokens) -> 
      case parseAexp moreTokens of
        Just (exp2, moreTokens2) -> Just (Eqa' exp1 exp2, moreTokens2)
        _ -> error "Run-time error"
    Just(exp1, LeTok : moreTokens) -> 
      case parseAexp moreTokens of
        Just (exp2, moreTokens2) -> Just (Le' exp1 exp2, moreTokens2)
        _ -> error "Run-time error"
    _ -> error "Run-time error"

parseBexp :: [Token] -> Maybe(Bexp, [Token])
parseBexp tokens = 
  case parseEqb tokens of
    Just(re, moreTokens) -> extendAnd (re, moreTokens)
    _ -> Nothing
extendAnd :: (Bexp, [Token]) -> Maybe(Bexp, [Token])
extendAnd (e1, AndTok: afterTokens) = 
  case parseEqb afterTokens of
    Just (e2, moreTokens) -> extendAnd (And' e1 e2, moreTokens)
    Nothing -> Just (e1, afterTokens)
extendAnd (e1, t : restTokens) = Just(e1, t : restTokens)
extendAnd (e1, []) = Just(e1, [])

parseEqb :: [Token] -> Maybe(Bexp, [Token])
parseEqb tokens = 
  case parseBool tokens of 
    Just(re, moreTokens) -> extendEqb (re, moreTokens)
    _ -> Nothing
extendEqb :: (Bexp, [Token]) -> Maybe(Bexp, [Token])
extendEqb (e1, EqbTok: afterTokens) = 
  case parseBool afterTokens of
    Just (e2, moreTokens) -> extendEqb (Eqb' e1 e2, moreTokens)
    Nothing -> Just (e1, afterTokens)
extendEqb (e1, t : restTokens) = Just(e1, t : restTokens)
extendEqb (e1, []) = Just(e1, [])

parseStm :: [Token] -> Maybe(Program, [Token])
parseStm (VarTok v : AttribTok : tokens) =
  case parseAexp tokens of 
    Just (re, moreTokens) -> Just([Attrib v re], moreTokens)
    _ -> error "Run-time error" 
parseStm (OpenTok : restTokens) =
  case extendProg ([], restTokens) of
    Just (prog, CloseTok : moreTokens) -> Just (prog, moreTokens)
    _ -> error "Run-time error"
parseStm (IfTok : restTokens) = 
  case parseBexp restTokens of
    Just (b1, ThenTok : afterThen) ->
      case parseStm afterThen of
        Just (e1, ElseTok : afterElse) ->
          case parseStm afterElse of
            Just (e2, moreTokens) -> Just([If b1 e1 e2], moreTokens)
            _ -> error "Run-time error"
        _ -> error "Run-time error"
    _ -> error "Run-time error"
parseStm (WhileTok : restTokens) =
  case parseBexp restTokens of
    Just (b1, DoTok : afterDo) ->
      case parseStm afterDo of
        Just (exp, moreTokens) -> Just([While b1 exp], moreTokens)
        _ -> error "Run-time error"
    _ -> error "Run-time error"
parseStm ([]) = Just([], [])
parseStm tokens = Nothing

parseProg :: [Token] -> Program
parseProg tokens = prog where Just(prog, _) = extendProg ([], tokens)
extendProg :: (Program, [Token]) -> Maybe(Program, [Token])
extendProg (prog, tokens)
  | length tokens > 0 = 
    case parseStm tokens of 
      Just (s, moreTokens) -> extendProg(prog ++ s, moreTokens)
      _ -> Just (prog, tokens)
  | otherwise = Just(prog, [])

parse :: String -> Program
parse str = 
  parseProg (lexer str)

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")