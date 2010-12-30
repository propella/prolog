-- Copyright (c) 2009 Takashi Yamamiya

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

-- | A prolog interpreter.
module Prolog
    (-- * Data structures
     Term(..), Clause(..),
     -- * Utility constructors for debugging
     w, s, cons,
     -- * Reader
     parse, parse',
     atom, variable, struct, list, nil, terms, arguments, term, clause, clauses, query,
     -- * Printer
     display,
     -- * Unification
     unify, unifyList, applyTerm,
     -- * Solver
     prove, rename,
     -- * Testing
     solveString, start) where

import Text.ParserCombinators.Parsec
import Data.Maybe (maybeToList)
import Char (isUpper)

infix 6 :-
data Term    = Var String Int | Struct String [Term] deriving (Show, Eq)
data Clause  = Term :- [Term]                        deriving (Show, Eq)
data Command = Fact Clause | Query [Term] | ShowAll | Noop

type Rules = [Clause]

-- Utility constructors for debugging
w :: String -> Term
w s@(x:xs) | isUpper x = Var s 0
           | otherwise = Struct s []

s :: String -> [Term] -> Term
s n xs = Struct n xs

cons s cdr = (Struct "cons" [w s, cdr])

---- Unification ----

type Substitution = [(Term, Term)]
true = []

-- | > apply [(w"X", w"Y"), (w"Y", w"Z")] [(w"X"), (w"Y")] == [(w"Z"), (w"Z")]
apply :: Substitution -> [Term] -> [Term]
apply s ts = [applyTerm s t | t <- ts]

applyTerm [] (Var y n)                                  = Var y n
applyTerm ((Var x i, t):s) (Var y j) | x == y && i == j = applyTerm s t
                                     | otherwise        = applyTerm s (Var y j)
applyTerm s (Struct n ts)                               = Struct n (apply s ts)

-- | > unify (w"X") (w"apple") == Just [(w"X", w"apple")]
unify :: Term -> Term -> Maybe Substitution
unify (Var x n) (Var y m) = Just [(Var x n, Var y m)]
unify (Var x n)      y    = Just [(Var x n,       y)]
unify      x    (Var y m) = Just [(Var y m,       x)]
unify (Struct a xs) (Struct b ys)
      | a == b = unifyList xs ys
      | otherwise   = Nothing

unifyList :: [Term] -> [Term] -> Maybe Substitution
unifyList [] [] = Just true
unifyList [] _ = Nothing
unifyList _ [] = Nothing
unifyList (x:xs) (y:ys) = do s <- unify x y
                             s' <- unifyList (apply s xs) (apply s ys)
                             return (s ++ s')

---- Solver ----

prove :: Rules -> [Term] -> [Substitution]
prove rules goals = find rules 1 goals

-- Depth first search
-- > find (parse' clauses "p(X):-q(X). q(a).") 1 [parse' term "p(X)"]
find :: Rules -> Int -> [Term] -> [Substitution]
find rules i [] = [true]
find rules i goals = do let rules' = rename rules i
                        (s, goals') <- branch rules' goals
                        solution <- find rules (i + 1) goals'
                        return (s ++ solution)

-- Find next branches. A branch is a pair of substitution and next goals.
-- > branch (parse' clauses "n(z). n(s(X)):-n(X).") (parse' query "?-n(X).")
branch :: Rules -> [Term] -> [(Substitution, [Term])]
branch rules (goal:goals) = do head :- body <- rules
                               s <- maybeToList (unify goal head)
                               return (s, apply s (body ++ goals))

-- | Rename all variables in the rules to split namespaces.
rename :: Rules -> Int -> Rules
rename rules i = [ renameVar head :- renameVars body | head :- body <- rules]
    where renameVar (Var s _)     = Var s i
          renameVar (Struct s ts) = Struct s (renameVars ts)
          renameVars ts           = [renameVar t | t <- ts]

---- Reader ----

-- Spaces are always consumed with the previous token.

parse' parser s = result where Right result = parse parser "" s
nil = Struct "nil" []

schar c = char c >> spaces
special = oneOf ":;+=-*&$#@/.~!" <|> digit

atom = (lower >>= \x -> many alphaNum >>= \xs -> spaces >> return (x:xs)) <|>
       (many1 special >>= \x -> spaces >> return x)

variable = upper >>= \x -> many alphaNum >>= \xs -> spaces >> return (Var (x:xs) 0)

struct = atom >>= \name -> arguments >>= \ls -> return (Struct name ls)

arguments = ((schar '(' >> terms >>= \ls -> schar ')' >> return ls)) <|>
            (spaces >> return [])

list = schar '[' >> terms >>= \ts -> listTail >>= \t -> return (makeList ts t)
    where makeList [] cdr     = cdr
          makeList (x:xs) cdr = Struct "cons" [x, makeList xs cdr]

listTail = (schar '|' >> term >>= \t -> schar ']' >> return t) <|>
           (schar ']' >> return nil)

term = (variable <|> struct <|> list) >>= \t -> return t
terms = sepBy term (schar ',')

clause = struct >>= \head -> ((schar '.' >> return (head :- [])) <|>
                              (query >>= \goals -> return (head :- goals)))
clauses = many clause

arrow = (char '?' <|> char ':') >> schar '-'
query = arrow >> terms >>= \goals -> schar '.' >> return goals

noop = (char '%' >> skipMany anyToken) <|> eof

command :: Parser Command
command = spaces >>
          ((clause >>= \c -> return (Fact c)) <|>
           try (query >>= \ts -> return (Query ts)) <|>
           (string "??" >> return (ShowAll)) <|>
           (noop >> return (Noop)))

-- parse atom "" "atom1234"
-- parse variable "" "Variable1234"
-- parse struct "" "father ( masuo , tara ) "
-- parse arguments "" "( orange , Apple , banana ) "
-- parse list "" "[]"
-- parse list "" "[ 1 , 2 | 3 ] "
-- parse terms "" "orange , apple , banana "
-- parse term "" "someAtom "
-- parse clause "" "child ( X , Y) :- mother( Y, X ) . "
-- parse query "" "?- apple ."

---- Printer ----

class Display a where
    displays :: a -> String -> String
    display :: a -> String
    display x = displays x ""

instance Display Term where
    displays (Var s 0)           = showString s
    displays (Var s n)           = showString s . showChar '_' . shows n
    displays (Struct "nil" [])     = showString "[]"
    displays (Struct "cons" [h, t]) = showChar '[' . displays h . displaysTail t . showChar ']'
    displays (Struct s [])       = showString s
    displays (Struct s xs)       = showString s . showChar '(' . displays xs . showChar ')'

displaysTail (Struct "nil" [])     = id
displaysTail (Struct "cons" [h, t]) = showChar ',' . displays h . displaysTail t
displaysTail x                   = showChar '|' . displays x

instance Display Clause where
    displays (head :- []) = displays head . showChar '.'
    displays (head :- bodies) = displays head . showString " :- " . displays bodies . showChar '.'

instance Display a => Display [a]  where
    displays []     = id
    displays [x]    = displays x
    displays (x:xs) = displays x . showChar ',' . displays xs

instance (Display a, Display b) => Display (a, b) where
    displays (x, y) = displays x . showChar '=' . displays y

displayLines []     = ""
displayLines (x:xs) = display x ++ "\n" ++ display xs

-- display (s"cons" [w"1", (s"cons" [w"2", nil])])
-- display ((s"child" [w"X",w"Y"]) :- [s"mother" [w"Y",w"X"]])

---- REPL --

main = interact start
start = writeStr ("food(apple). -- Add a clause.\n" ++
                  "?- food(X).  -- Query.\n" ++
                  "??           -- Show all.\n\n") (loop [])

loop :: Rules -> String -> String
loop rules = readLine (exec rules . parse command "")

exec :: Rules -> Either ParseError Command -> String -> String
exec rules (Right (Fact c))  = writeStr ("=> " ++ display c ++ "\n" ) (loop (rules ++ [c]))
exec rules (Right (Query q)) = answer q (prove rules q) rules
exec rules (Right ShowAll)   = writeStr (showAll rules) (loop rules)
exec rules (Right Noop)      = loop rules
exec rules (Left e)          = writeStr (show e ++ "\n") (loop rules)

answer :: [Term] -> [Substitution] -> Rules -> String -> String
answer q [] rules     = writeStr "No\n" (loop rules)
answer q (c:cs) rules = writeStr ("=> " ++ result ++ "\n") (more q cs rules)
    where result = display (apply c q)

more :: [Term] -> [Substitution] -> Rules -> String -> String
more q cs rules = readLine f
    where f (';':_) = answer q cs rules
          f x   = writeStr "Yes\n" (loop rules)

showAll rules = [line | r <- rules, line <- "=> " ++ display r ++ "\n" ]

-- Interactive library

-- Its arguments are a string to be written and next process.
writeStr :: String -> (String -> String) -> (String -> String)
writeStr output proc input = output ++ proc input

-- Its argument is a process which receives a line.
readLine :: (String -> (String -> String)) -> (String -> String)
readLine proc input = case (nextLine input) of
                        ("", [])      -> "" -- End of file
                        (line, rest) -> proc line rest

nextLine ""        = ("","")
nextLine ('\n':xs) = ("\n", xs)
nextLine (x:xs)    = (x:ys, zs) where (ys, zs) = nextLine xs

---- Testing ----

-- | Test function
-- 
-- >>> solveString "p:-q. q:-r. r." "?-p."
-- > [[]]
-- >>> solveString' "p(X):-q(X).q(a)." "?-p(X)."
-- > ["X=X_1,X_1=a"]

solveString :: String -> String -> [Substitution]
solveString rules q =
    let rules' = parse' clauses rules
        q' = parse' query q
    in prove rules' q'

solveString' rules q = [display s | s <- solveString rules q]
