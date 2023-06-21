module LambdaParser where

import Parser
import Data.Lambda
import Data.Builder
import Data.Char
import Data.Bool
import Control.Applicative

-- You can add more imports if you need them

-- Remember that you can (and should) define your own functions, types, and
-- parser combinators. Each of the implementations for the functions below
-- should be fairly short and concise.


{-|
    Part 1
-}

-- | Exercise 1

-- | Parses a string representing a lambda calculus expression in long form
-- 
-- >>> parse longLambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse longLambdaP "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse longLambdaP "(λx(λy.x))"
-- UnexpectedChar '('

-- make for "\x.x" first

-- alphabet :: Parser Char
-- alphabet = satisfy is

bracketR :: Parser Char
bracketR = is '('

bracketL :: Parser Char
bracketL =  is ')'

lambda :: Parser String
lambda =  string "\955"

alphabet :: Parser Char
alphabet = satisfy isLower

contAlpha :: Parser [Char]
contAlpha = list alphabet


repeatSequenceL :: Parser Builder
repeatSequenceL = 
    sequenceL 
    |||
    (do
        x <- contAlpha
        y <- parseBracketChars
        pure $ (Data.Builder.ap) (foldl1 (Data.Builder.ap) (Prelude.map term x)) (y)
        )
    |||
    parseBracketChars
    |||
    (foldl1 Data.Builder.ap <$> (fmap(fmap(term))contAlpha))
    
parseBracketChars :: Parser Builder
parseBracketChars = (do
    bracketR
    x <- contAlpha
    bracketL
    pure(foldl1 Data.Builder.ap (Prelude.map term x))
    )
    |||
    sequenceL

sequenceL :: Parser Builder
sequenceL = do
    bracketR
    lambda
    x <- alphabet
    is '.'
    z <- repeatSequenceL
    bracketL
    pure (lam x z)


longLambdaP :: Parser Lambda
longLambdaP = (<$>) build sequenceL

-- | Parses a string representing a lambda calculus expression in short form
--
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy
--
-- >>> parse shortLambdaP "λxyz"
-- UnexpectedEof

withBracketShortLambdaP :: Parser Builder
withBracketShortLambdaP = (do
    bracketR
    lambda
    x <- contAlpha
    is '.'
    y <- (foldl1 Data.Builder.ap <$> (fmap(fmap(term))contAlpha))
    bracketL
    z <- withBracketShortLambdaP
    pure(Data.Builder.ap (foldr (lam) (y) (x)) (z))
    )
    |||
    (do
    bracketR
    lambda
    x <- contAlpha
    is '.'
    y <- (foldl1 Data.Builder.ap <$> (fmap(fmap(term))contAlpha))
    bracketL
    pure(foldr (lam) (y) (x))
    )


repeatSequenceS :: Parser Builder
repeatSequenceS = 
    withoutBracketShortLambdaP
    |||
    withBracketShortLambdaP
    |||
    (do
        x <- contAlpha
        y <- withBracketShortLambdaP
        pure $ (Data.Builder.ap) (foldl1 (Data.Builder.ap) (Prelude.map term x)) (y)
    )
    |||
    (do
        x <- contAlpha
        y <- parseBracketChars
        pure $ (Data.Builder.ap) (foldl1 (Data.Builder.ap) (Prelude.map term x)) (y)
    )
    |||
    parseBracketChars
    |||
    (foldl1 Data.Builder.ap <$> (fmap(fmap(term))contAlpha))

withoutBracketShortLambdaP :: Parser Builder
withoutBracketShortLambdaP = 
    (do
    lambda
    x <- contAlpha
    is '.'
    y <- repeatSequenceS
    pure(foldr (lam) (y) (x))
    )

shortLambdaP :: Parser Lambda
shortLambdaP = (<$>) build withBracketShortLambdaP ||| (<$>) build withoutBracketShortLambdaP


-- | Parses a string representing a lambda calculus expression in short or long form
-- >>> parse lambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse lambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse lambdaP "λx..x"
-- UnexpectedChar '.'
--

lambdaP :: Parser Lambda
lambdaP = longLambdaP ||| shortLambdaP

{-|
    Part 2
-}

-- | Exercise 1

-- IMPORTANT: The church encoding for boolean constructs can be found here -> https://tgdwyer.github.io/lambdacalculus/#church-encodings

-- | Parse a logical expression and returns in lambda calculus
-- >>> lamToBool <$> parse logicP "True and False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse logicP "True and False or not False and True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "not not not False"
-- Result >< Just True
--
-- >>> parse logicP "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f) (\t_.t)\_f.f
--
-- >>> parse logicP "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True

logicP :: Parser Lambda
logicP = (<$>) build logicalParser

chkBool :: Parser Builder
chkBool = 
    (do
    string "True"
    pure(boolToLam True)
    )
    |||
    (do
    string "False"
    pure(boolToLam False)
    )


chkOperators :: Parser Builder
chkOperators = 
    (do
    string "not"
    pure(buildNot)
    )
    |||
    (do
    string "if"
    pure(buildIf)
    )

chkSOper :: Parser Builder
chkSOper = 
    (do
    string "and"
    pure(buildAnd)
    )
    |||
    (do
    string "or"
    pure(buildOr)
    )


logicalParser :: Parser Builder
logicalParser = 
    (do
        x <- chkBool
        space 
        y <- chkSOper
        space
        z <- logicalParser
        pure(y `ap` x `ap` z)
    )
    |||
    (do
        x <- chkBool
        space 
        y <- logicalParser
        pure(x `ap` y)
    )
    |||
    (do
        x <- chkOperators
        space 
        y <- logicalParser
        pure(x `ap` y)
    )
    -- xxxxxxxxxxxxx
    |||
    (do
        x <- chkBool
        pure(x)
    )
    |||
    (do
        x <- chkOperators
        pure(x)
    )


buildIf :: Builder
buildIf = lam 'b' $ lam 't' $ lam 'f' ((term 'b') `ap` (term 't') `ap` (term 'f'))

buildAnd :: Builder
buildAnd = lam 'x' $ lam 'y' ((buildIf) `ap` (term 'x') `ap` (term 'y') `ap` (lam '_' $ lam 'f' (term 'f')))

-- λxy. IF x TRUE y
buildOr :: Builder
buildOr = lam 'x' $ lam 'y' ((buildIf) `ap` (term 'x') `ap` (lam 't' $ lam '_' (term 't')) `ap` (term 'y') ) 

-- λx. IF x FALSE TRUE
buildNot :: Builder
buildNot = lam 'x' ((buildIf) `ap` (term 'x') `ap` (lam '_' $ lam 'f' (term 'f')) `ap` (lam 't' $ lam '_' (term 't')))

-- | Exercise 2

-- | The church encoding for arithmetic operations are given below (with x and y being church numerals)

-- | x + y = add = λxy.y succ m
-- | x - y = minus = λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx

-- | The helper functions you'll need are:
-- | succ = λnfx.f(nfx)
-- | pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
-- | Note since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

-- | Parse simple arithmetic expressions involving + - and natural numbers into lambda calculus
-- >>> lamToInt <$> parse basicArithmeticP "5 + 4"
-- Result >< Just 9
--
-- >>> lamToInt <$> parse basicArithmeticP "5 + 9 - 3 + 2"
-- Result >< Just 13

buildAdd :: Builder -> Builder -> Builder
buildAdd x y = (y `ap` LambdaParser.succ) `ap` x 

buildMinus :: Builder -> Builder -> Builder
buildMinus x y = (y `ap` LambdaParser.pred) `ap` x

buildMultiply :: Builder -> Builder -> Builder
-- | x * y = multiply = λxyf.x(yf)
buildMultiply x y = lam 'f' (x `ap` (y `ap` term 'f'))

buildPower :: Builder -> Builder -> Builder
-- | x ** y = exp = λxy.yx
buildPower = flip ap

succ :: Builder
succ = lam 'n' $ lam 'f' $ lam 'x' ((term 'f') `ap` (term 'n' `ap` term 'f' `ap` term 'x'))

pred :: Builder
pred = lam 'n' $ lam 'f' $ lam 'x' ((term 'n') `ap` (lam 'g' $ lam 'h' ((term 'h') `ap` (term 'g' `ap` term 'f'))) `ap` (lam 'u' (term 'x')) `ap` (lam 'u' (term 'u'))  )

digit :: Parser Char
digit = satisfy isDigit

numbers :: Parser [Char]
numbers = list digit

-- xxxxxxxxxxxxxxxxxxxxxxxxxxx
chain :: Parser a -> Parser (a->a->a) -> Parser a
chain p op = p >>= rest
   where
   rest a = (do
               f <- op
               b <- p
               rest (f a b)
            ) ||| pure a


op :: Char -> Parser Char -- parse a single char operator
op c = do
   spaces
   is c
   pure c

chkNumbers :: Parser Builder
chkNumbers = do
    spaces
    a <- munch1 isDigit
    pure(intToLam $ Prelude.read a)

add :: Parser (Builder -> Builder -> Builder)
add = do
    op '+'
    pure(buildAdd)

minus :: Parser (Builder -> Builder -> Builder)
minus = do
    op '-'
    pure(buildMinus)


-- | Expression consisting of terms and lower precedence operators
exprB :: Parser Builder
exprB = chain chkNumbers (add ||| minus)


basicArithmeticP :: Parser Lambda
basicArithmeticP = (<$>) build exprB

-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68

-- expr :: Parser Expr
-- expr = chain term add

-- term :: Parser Expr
-- term = chain number times

multiply :: Parser (Builder -> Builder -> Builder)
multiply = do
    op '*'
    pure(buildMultiply)

power :: Parser (Builder -> Builder -> Builder)
power = do
    string "**"
    pure(buildPower)

bracketExpr :: Parser Builder
bracketExpr = is '(' *> aExpr <* is ')'

aExpr :: Parser Builder
aExpr = chain termAMultiply (add ||| minus)

termAPower :: Parser Builder
termAPower = chain (chkNumbers ||| bracketExpr) (power)

termAMultiply :: Parser Builder
termAMultiply = chain (termAPower ||| chkNumbers ||| bracketExpr) (multiply)

arithmeticP :: Parser Lambda
arithmeticP = (<$>) build aExpr


-- | Exercise 3

-- | The church encoding for comparison operations are given below (with x and y being church numerals)

-- | x <= y = LEQ = λmn.isZero (minus m n)
-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)

-- | The helper function you'll need is:
-- | isZero = λn.n(λx.False)True

-- >>> lamToBool <$> parse complexCalcP "9 - 2 <= 3 + 6"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
-- Result >< Just False
complexCalcP :: Parser Lambda
complexCalcP = undefined


{-|
    Part 3
-}

-- | Exercise 1

-- | The church encoding for list constructs are given below
-- | [] = null = λcn.n
-- | isNull = λl.l(λht.False) True
-- | cons = λhtcn.ch(tcn)
-- | head = λl.l(λht.h) False
-- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)
--
-- >>> parse listP "[]"
-- Result >< \cn.n
--
-- >>> parse listP "[True]"
-- Result >< (\htcn.ch(tcn))(\xy.x)\cn.n
--
-- >>> parse listP "[0, 0]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
--
-- >>> parse listP "[0, 0"
-- UnexpectedEof
listP :: Parser Lambda
listP = undefined

-- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head rest [True, False, True, False, False]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull []"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- Result >< Just False
listOpP :: Parser Lambda
listOpP = undefined


-- | Exercise 2

-- | Implement your function(s) of choice below!
