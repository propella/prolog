import Test.HUnit
import Prolog

main = runTestTT allTest

testParser p input = case (parse p "" input) of
                       Left err -> Left (show err)
                       Right x -> Right x

allTest = test [
           "list" ~: listTest,
           "solver" ~: solverTest,
           "parser" ~: parserTest,
           "printer" ~: printerTest,
           "unification"  ~: unificationTest
          ]

-- Tests

solvedValue rules q i v = applyTerm (solveString rules q !! i) v

listTest = test [
              "[]" ~: parse' list "[]" ~=? nil,
              "[1]" ~: parse' list "[ 1 ] " ~=? (cons "1" nil),
              "[1,2]" ~: parse' list "[ 1 , 2 ] " ~=? (cons "1" (cons "2" nil)),
              "[1|[2]]" ~: parse' list "[ 1 | [ 2 ] ] " ~=? (cons "1" (cons "2" nil)),
              "[1,2|3]" ~: parse' list "[ 1 , 2 | 3 ] " ~=? (cons "1" (cons "2" (w"3"))),
              "[1,2|[3,4]]" ~: parse' list "[ 1 , 2 | [ 3 , 4 ] ] " ~=?
                         (cons "1" (cons "2" (cons "3" (cons "4" nil))))
              ]

solverTest = test [
              "fact1" ~: prove [w"apple" :- [], w"orange" :- []] [w"banana"] ~=? [],
              "fact2" ~: prove [w"apple" :- [], w"orange" :- []] [w"apple"] ~=? [[]],
              "transitive" ~: solveString "p:-q. q:-r. r." "?-p." ~=? [[]],
              "variable" ~: solveString "p(a)." "?-p(X)." ~=? [[(w"X", w"a")]],
              "rename" ~: rename (parse' clauses "p(X):-q(X).") 1 ~=?
                           [s"p" [Var "X" 1] :- [s"q" [Var "X" 1]]],
              "variable" ~: solvedValue "p(X):-q(X).q(a)." "?-p(X)." 0 (w"X")
                             ~=? (w"a"),
              "number" ~: solvedValue "n(z). n(s(X)):-n(X)." "?-n(X)." 5 (w"X")
                             ~=? (parse' term "s(s(s(s(s(z))))).")
             ]

unificationTest = test [
              "apply1" ~: applyTerm [(w"X", w"Y"), (w"Y", w"Z")] (w"X") ~=? (w"Z"),
              "apply2" ~: applyTerm [(w"X",Var "X" 1)] (Var "X" 1) ~=? (Var "X" 1),
              "list" ~: unifyList [w"X",w"Y",w"Y"] [w"Z",w"Z",w"a"] ~=?
                     Just [(w"X",w"Z"), (w"Y",w"Z"), (w"Z",w"a")]
                  ]
parserTest = test [
              "atom" ~:  testParser atom "atom1234" ~=? Right "atom1234",
              "variable" ~:  testParser variable "Var1234" ~=? Right (w"Var1234"),

              "struct1" ~:  testParser struct "father(masuo, tara)" ~=?
                        Right (s"father" [w"masuo", w"tara"]),
              "struct2" ~:  testParser struct "father" ~=?
                        Right (w"father"),
              "struct3" ~:  testParser struct "father ( masuo , tara )" ~=?
                        Right (s"father" [w"masuo", w"tara"]),

              "terms1" ~:  testParser terms "orange, Apple, Banana" ~=?
                       Right [w"orange", w"Apple", w"Banana"],
              "terms2" ~:  testParser terms "orange , Apple , Banana " ~=?
                       Right [w"orange", w"Apple", w"Banana"],

              "arguments1" ~:  (testParser arguments "( orange , Apple , Banana )") ~=?
                           Right [w"orange", w"Apple", w"Banana"],
              "arguments2" ~:  (testParser arguments " ") ~=?
                           Right [],

              "term1" ~:  (testParser term "orange ") ~=? Right (w"orange"),
              "term2" ~:  (testParser term "Orange ") ~=? Right (w"Orange"),

              "clause1" ~:  (testParser clause "head :- body .") ~=?
                        Right (w"head" :- [w"body"]),
              "clause2" ~:  (testParser clause "head .") ~=?
                        Right (w"head" :- []),
              "clause3" ~:  (testParser clause "child(X, Y) :- mother(Y, X).") ~=?
                        Right (s"child" [w"X",w"Y"] :- [s"mother" [w"Y",w"X"]]),

              "query1" ~:  (testParser query "?- apple .") ~=? Right [w"apple"],
              "query2" ~:  (testParser query "?- apple , orange .") ~=? Right [w"apple", w"orange"]

             ]

printerTest = test [
               "display1" ~:  display (w"Hello") ~=? "Hello",
               "display2" ~:  display (w"hello") ~=? "hello",
               "display3" ~:  display (s"mother" [w"sazae", w"tara"]) ~=?
                         "mother(sazae,tara)",
               "display4" ~:  display (w"head" :- []) ~=? "head.",
               "display5" ~:  display (s"child" [w"X",w"Y"] :- [s"mother" [w"Y",w"X"]]) ~=?
                         "child(X,Y) :- mother(Y,X)."

             ]
