# NUMEX-Interpreter
A pure functional implementation of NUMEX (Number Expression Programming Language) written in Racket
---
__Example Command__:
```lisp
(eval-exp (with "f1" (lam "f1" "a" (with "x" (var "a") (lam "f2" "z" (plus (var "x") (num 1)))))
                               (with "f3" (lam "f3" "f" (with "x" (num 1729) (apply (var "f") (munit)))) 
                                     (apply (var "f3") (apply (var "f1") (num 1))))))
```

__Output__:
```
(num 2)
```
