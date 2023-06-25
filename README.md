# Made-Up-Programming-Language-MUPL-Interpreter

## Syntax

MUPL programs are written directly in Racket by using the constructors defined by the structs defined at the beginning of MUPL.rkt. This is the definition of mupl’s syntax:
- If s is a Racket string, then (var s) is a mupl expression (a variable use).
- If n is a Racket integer, then (int n) is a mupl expression (a constant).
- If e1 and e2 are mupl expressions, then (add e1 e2) is a mupl expression (an addition).
- If e1 and e2 are mupl expressions, then (isgreater e1 e2) is a mupl expression (a comparison).
- If e1, e2, and e3 are mupl expressions, then (ifnz e1 e2 e3) is a mupl expression. It is a condition where the result is e2 if e1 is not zero else the result is e3. Only one of e2 and e3 is evaluated.
- If s1 and s2 are Racket strings and e is a mupl expression, then (fun s1 s2 e) is a mupl expression (a function). In e, s1 is bound to the function itself (for recursion) and s2 is bound to the (one) argument. Also, (fun null s2 e) is allowed for anonymous nonrecursive functions.
- If e1 and e2 are mupl expressions, then (call e1 e2) is a mupl expression (a function call).
- If s is a Racket string and e1 and e2 are mupl expressions, then (mlet s e1 e2) is a mupl expression
(a let expression where the value resulting from evaluating e1 is bound to s in the evaluation of e2).
- If e1 and e2 are mupl expressions, then (apair e1 e2) is a mupl expression (a pair-creator).
- If e1 is a mupl expression, then (first e1) is a mupl expression (getting the first part of a pair).
- If e1 is a mupl expression, then (second e1) is a mupl expression (getting the second part of a pair).
- (munit) is a mupl expression (holding no data, much like null or ’() in Racket). Notice that (munit) is a mupl expression, but munit is not.
- If e1 is a mupl expression, then (ismunit e1) is a mupl expression (testing for (munit)).
- (closure env f) is a mupl value where f is mupl function (an expression made from fun) and env is an environment mapping variables to values. Closures do not appear in source programs; they result from evaluating functions.

## More Details

