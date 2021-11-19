import Data.Ratio
import Data.Char

data Token = Simple SimpleToken     | 
             Compound CompoundToken | 
             LexError String        deriving (Eq, Show)

data SimpleToken = EOF   | 
                   COMMA | 
                   PLUS  | 
                   MINUS | 
                   STAR  | 
                   SLASH | 
                   EQ1   | 
                   OP    | 
                   CP    | 
                   LET   | 
                   IN
                   deriving (Eq, Show)

data CompoundToken = Id String | Num Rational deriving (Eq, Show)

stateStart :: [Char] -> [Char] -> (Token -> [Char] -> t) -> t
stateStart "" seen k = k (Simple EOF) ""
stateStart (',':cs) seen k = stateHasComma cs (',':seen) k
stateStart ('+':cs) seen k = stateHasPlus cs ('+':seen) k
stateStart ('-':cs) seen k = stateHasMinus cs ('-':seen) k
stateStart ('*':cs) seen k = stateHasStar cs ('*':seen) k
stateStart ('/':cs) seen k = stateHasSlash cs ('/':seen) k
stateStart ('=':cs) seen k = stateHasEq cs ('=':seen) k
stateStart ('(':cs) seen k = stateHasOP cs ('(':seen) k
stateStart (')':cs) seen k = stateHasCP cs (')':seen) k
stateStart ('l':cs) seen k = stateHasL cs ('l':seen) k
stateStart ('i':cs) seen k = stateHasI cs ('i':seen) k
stateStart (c:cs) seen k 
  | isSpace c = stateStart cs seen k
  | isAlpha c = stateHasAlpha cs (c:seen) k
  | isDigit c = stateHasNum cs (c:seen) k
  | otherwise = k (LexError ("Unexpected character: " ++ [c])) ""

stateHasComma :: [Char] -> [Char] -> (Token -> [Char] -> t) -> t
stateHasComma cs seen k = k (Simple COMMA) cs

stateHasPlus :: [Char] -> [Char] -> (Token -> [Char] -> t) -> t
stateHasPlus cs seen k = k (Simple PLUS) cs

stateHasMinus :: [Char] -> [Char] -> (Token -> [Char] -> t) -> t
stateHasMinus cs seen k = k (Simple MINUS) cs

stateHasStar :: [Char] -> [Char] -> (Token -> [Char] -> t) -> t
stateHasStar cs seen k = k (Simple STAR) cs

stateHasSlash :: [Char] -> [Char] -> (Token -> [Char] -> t) -> t
stateHasSlash cs seen k = k (Simple SLASH) cs

stateHasEq :: [Char] -> [Char] -> (Token -> [Char] -> t) -> t
stateHasEq cs seen k = k (Simple EQ1) cs

stateHasOP :: [Char] -> [Char] -> (Token -> [Char] -> t) -> t
stateHasOP cs seen k = k (Simple OP) cs

stateHasCP :: [Char] -> [Char] -> (Token -> [Char] -> t) -> t
stateHasCP cs seen k = k (Simple CP) cs

stateHasL :: [Char] -> [Char] -> (Token -> [Char] -> t) -> t
stateHasL "" seen k = k (Compound (Id (reverse seen))) ""
stateHasL ('e':cs) seen k = stateHasLE cs ('e':seen) k
stateHasL (c:cs) seen k 
   | isAlpha c      = stateHasAlpha cs (c:seen) k
   | otherwise      = k (Compound (Id (reverse seen))) (c:cs)

stateHasLE :: [Char] -> [Char] -> (Token -> [Char] -> t) -> t
stateHasLE "" seen k = k (Compound (Id (reverse seen))) ""
stateHasLE ('t':cs) seen k = stateHasLET cs ('t':seen) k
stateHasLE (c:cs) seen k 
   | isAlpha c      = stateHasAlpha cs (c:seen) k
   | otherwise      = k (Compound (Id (reverse seen))) (c:cs)

stateHasLET :: [Char] -> [Char] -> (Token -> [Char] -> t) -> t
stateHasLET "" seen k = k (Simple LET) ""
stateHasLET (c:cs) seen k 
   | isAlpha c      = stateHasAlpha cs (c:seen) k
   | otherwise      = k (Simple LET) (c:cs)

stateHasI :: [Char] -> [Char] -> (Token -> [Char] -> t) -> t
stateHasI "" seen k = k (Compound (Id (reverse seen))) ""
stateHasI ('n':cs) seen k = stateHasIN cs ('n':seen) k
stateHasI (c:cs) seen k 
   | isAlpha c      = stateHasAlpha cs (c:seen) k
   | otherwise      = k (Compound (Id (reverse seen))) (c:cs)

stateHasIN :: [Char] -> [Char] -> (Token -> [Char] -> t) -> t
stateHasIN "" seen k = k (Simple IN) ""
stateHasIN (c:cs) seen k 
   | isAlpha c      = stateHasAlpha cs (c:seen) k
   | otherwise      = k (Simple IN) (c:cs)


stateHasAlpha :: [Char] -> [Char] -> (Token -> [Char] -> t) -> t
stateHasAlpha "" seen k = k (Compound (Id (reverse seen))) ""
stateHasAlpha (c:cs) seen k 
   | isAlpha c      = stateHasAlpha cs (c:seen) k
   | otherwise      = k (Compound (Id (reverse seen))) (c:cs)


stateHasNum :: [Char] -> [Char] -> (Token -> [Char] -> t) -> t
stateHasNum "" seen k = k (Compound (Num (read (reverse seen)))) ""
stateHasNum (c:cs) seen k 
   | isDigit c      = stateHasNum cs (c:seen) k
   | otherwise      = k (Compound (Num (read (reverse seen)))) (c:cs)


getToken :: [Char] -> (Token -> [Char] -> t) -> t
getToken s k = stateStart s "" k

-- token stream constructor

tokenStreamFromString :: [Char] -> [Token]
tokenStreamFromString s = let k t s = t:(getToken s k)
                          in getToken s k

-- token stream interface

isEmptyTokenStream :: [Token] -> Bool
isEmptyTokenStream (t:ts) = (t == (Simple EOF))




data Exp = RExp Rational  | 
           Var String     | 
           Sum Exp Exp    | 
           Diff Exp Exp   | 
           Prod Exp Exp   |
           Quo Exp Exp    |
           Neg Exp        |
           Let EqnSeq Exp |
           ParseError String deriving (Eq, Show)


data EqnSeq = Seq [Eqns] | ParseEqnSeqError String deriving (Show, Eq)

data Eqns = Const Rational | Eqn Exp Exp deriving (Eq, Show)


stringFromToken :: Token -> String
stringFromToken (Compound (Id s)) = s

rationalFromToken :: Token -> Rational
rationalFromToken (Compound (Num n)) = n


newtype Parser a = Parser ([Token] -> [(a, [Token])])

unWrap :: Parser a -> [Token] -> [(a, [Token])]
unWrap (Parser f) = f

instance Monad Parser where
  return a = Parser(\ts->[(a, ts)])
  p >>= f  = Parser(\ts->[(b, ts2) | (a, ts1) <- unWrap p ts, (b, ts2) <- unWrap (f a) ts1])
--  fail s   = Parser(\ts-> [])


instance MonadFail Parser where
    fail s = Parser(\ts-> [])


instance Applicative Parser where
    pure  = return
    mf <*> ma = do f <- mf
                   a <- ma
                   return (f a)

instance Functor Parser where
  fmap g fx = (pure g) <*> fx

                                                                         

-- item makes sense for "token" Parsers
item :: Parser Token
item = Parser(\ts->case ts of [] -> []; t:ts1 -> [(t,ts1)])

parserFilter :: Parser b -> (b -> Bool) -> Parser b
parserFilter parser p = do {a <- parser; if p a then return a else (fail "Parser")}

literal :: Token -> Parser Token
literal t = parserFilter item (==t)

variable :: Parser Token
variable =  parserFilter item (\tok->case tok of (Compound (Id _)) -> True; _ -> False)

number :: Parser Token
number =  parserFilter item (\tok->case tok of (Compound (Num _)) -> True; _ -> False)

(+++) :: Parser a -> Parser a -> Parser a
p1 +++ p2 = Parser(\ts-> (unWrap p1 ts) ++ (unWrap p2 ts))

getDefs :: Parser EqnSeq
getDefs = getDefs2 []

getDefs2 :: [Eqns] -> Parser EqnSeq
getDefs2 revBinds =
  do binding <- getDef
     ((do tok <- (literal (Simple COMMA))
          getDefs2 (binding:revBinds))
       +++
      (return (Seq (reverse (binding:revBinds)))))

getDef :: Parser Eqns
getDef = 
  do
     exp1 <- getMathExp
     tok <- (literal (Simple EQ1))
     exp2 <- getMathExp
     return (Eqn exp1 exp2)

getMathExp :: Parser Exp
getMathExp = 
  do term <- getTerm
     getMathExp' term

getMathExp' :: Exp -> Parser Exp
getMathExp' term =
  (do tok <- (literal (Simple PLUS)) 
      term2 <- getTerm
      getMathExp' (Sum term term2))
  +++
  (do tok <- (literal (Simple MINUS))
      term2 <- getTerm
      getMathExp' (Diff term term2))
  +++
   (return term)

getTerm :: Parser Exp
getTerm = 
  do factor <- getFactor
     getTerm' factor

getTerm' :: Exp -> Parser Exp
getTerm' factor = 
  (do tok <- (literal (Simple STAR))
      factor2 <- getFactor
      getTerm' (Prod factor factor2))
  +++
  (do tok <- (literal (Simple SLASH))
      factor2 <- getFactor
      getTerm' (Quo factor factor2))
  +++
   (return factor)

getFactor :: Parser Exp
getFactor = 
  (do vtok <- variable
      return (Var (stringFromToken vtok)))
  +++
  (do ntok <- number
      return (RExp (rationalFromToken ntok)))
  +++
  (do tok <- (literal (Simple MINUS))
      factor <- getFactor
      return (Neg factor))
  +++
  (do tok <- literal (Simple OP) 
      exp <- getMathExp
      tok <- literal (Simple CP)
      return exp)

parse :: [Token] -> EqnSeq
parse ts =
  case unWrap getDefs ts of
    []            -> ParseEqnSeqError "Bad input" 
    (exp, ts1):ps -> if isEmptyTokenStream ts1
                     then exp
                     else ParseEqnSeqError "Unconsumed input" 

parseString :: String -> EqnSeq
parseString = parse . tokenStreamFromString