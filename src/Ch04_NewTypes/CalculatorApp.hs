module Ch04_NewTypes.CalculatorApp where

import Text.Read (readEither)
import Prelude hiding (Word)

{-
    NOTE: This app is very simple. It runs in the terminal console.
    It supports 4 basic arithmetic operators: Add, Sub, Mul and Div.
    The arithmetic expressions are in prefix notation. The expressions are
    "cleaned", i.e. operands and operators are space-delimited.

    > run "+ 3 5"
    "The answer is: 8"

    > run "/ 16 4"
    "The answer is: 4"

    > run "- 10 + 1 * 2 / 8 4"
    "The answer is: 5"

    NOTE: This is a simple example so all functions are monomorphic.
-}

{- | This co-product type expresses the recursive nature of an arithmetic expression. The bottom case is
an Integral _literal_ value, as in @Lit 2@ or @Lit 12@. Otherwise, it is another Expr (recursive) encoding
a supported infix operation.

The supported operations are Add, Sub, Mul, Div. An operation consists of an _operator_ and 2 _operand_
symbols, in that order.
-}
data Expr
    = Lit Int
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    deriving (Show, Eq, Ord)

{-
   NOTE: on the semantic meaning of the type aliases below.

   All the aliases below are underlied by the more universal type String. String is _the_ operational
   type _for us humans_ when we interface with the machine. In this example program, from the input, to
   the input broken into words for further processing, to the error messages -- the choice of String is
   the easiest and most natural type to work with. It is ubiquitous in that sense, and therefore carries
   that semantic meaning across all aliases.
-}
type Input = String
type ErrorMsg = String
type Word = String
type Output = String

{- | Evaluates an @Expr@ into the monomorphic primitive @Int@. This is the core evaluator of the co-product
type.

Named @eval@ in the book.

>>> evaluate $ Add (Lit 1) (Lit 2)
3
-}
evaluate :: Expr -> Int
evaluate expr =
    case expr of
        Lit num -> num
        Add a b -> apply (+) a b
        Sub a b -> apply (-) a b
        Mul a b -> apply (*) a b
        Div a b -> apply div a b
  where
    apply :: (Int -> Int -> Int) -> Expr -> Expr -> Int
    apply op a b = op (evaluate a) (evaluate b)

{-
   NOTE: the next 3 functions handle the the lexing and transforming of a `String` input into a
   valid `Expr` for the evaluator. Any error occurred during this process bubbles up to the top
   function `parse`.

   NOTE: a valid `Expr` might be produced through a composition of the functions `tokenize` and
   `makeInfixExpr` (which are mutually recursive). They are the core of building a valid `Expr`.

   NOTE: the function `parse` wraps `tokenize` and provides an another layer of error handling,
   validating that an `Expr` is only valid if the entire input is consumed.
-}

{- | Attempts to transform a series of input @Word@ into an @Expr@. Handles error appropriately. This
combinator handles 3 parsing cases.

All errors are handled on the Left branch. The Right branch splits further into 2 separate cases.
- A successful tokenization consumes the entire input.
- Otherwise, some extra token which cannot be made into a valid @Expr@ must exist, which bubbles up
    to the Left branch as an error.

>>> (parse . words) "+ 2 3"
Right (Add (Lit 2) (Lit 3))
-}
parse :: [Word] -> Either ErrorMsg Expr
parse ws =
    case tokenize ws of
        Left err -> Left err
        -- successfullly tokenized the entire input.
        Right (expr, []) -> Right expr
        -- some extra token/word must have been found, causing the input remainder to be non-empty.
        Right (_, rest) -> Left $ "Found extra tokens: " <> unwords rest

{- | Attempts to transform some words into an @Expr@. Tracks the remainder of the input. Mutually recursive
with @makeInfixExpr@.

Named @parse'@ in the book.

>>> (tokenize . words) "+ 2 3 - 5"
Right (Add (Lit 2) (Lit 3),["-","5"])
-}
tokenize :: [Word] -> Either ErrorMsg (Expr, [Word])
tokenize [] = Left "unexpected end of expression"
tokenize (word : rest) = case word of
    "+" -> makeInfixExpr Add rest
    "-" -> makeInfixExpr Sub rest
    "*" -> makeInfixExpr Mul rest
    "/" -> makeInfixExpr Div rest
    -- attempts to read and typecast a word into an Integer
    literal -> case readEither literal {- :: Either ErrorMsg Int -} of
        Left err -> Left err
        Right valid -> Right (Lit valid, rest)

{- | Attempts to transform an arithmetic function and some words into an @Expr@. Mutually recursive
with @tokenize@.

Named @parseBinary@ in the book.
-}
makeInfixExpr :: (Expr -> Expr -> Expr) -> [Word] -> Either ErrorMsg (Expr, [Word])
makeInfixExpr f _words = do
    (a, as) <- tokenize _words
    (b, bs) <- tokenize as
    Right (f a b, bs)

{-
   NOTE: the function `run` finally composes the functions above into a single action. It evaluates
   a valid `Expr` but abstracting away the underlying details into a single transformation, from a
   user input `String` into a formatted output `String`.
-}

{- | Parses a @String@ input and transforms it into a formatted @String@ output.

This combinator is composed from the @parse@ and @evaluate@ combinators. Any errors arisen from the process
will bubble up to the top (this combinator) and handled on the Left branch. Otherwise, the result is a
valid @Expr@ on the Right branch, which is promptly computed and embedded in a formatted String.
-}
run :: Input -> Output
run expr =
    case parse (words expr) of
        Left err -> "Error: " <> err
        Right valid ->
            let answer = show $ evaluate valid
             in "The answer is: " <> answer
