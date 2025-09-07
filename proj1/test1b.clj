; test1b.clj      09 Sep 24
; Alternate version that shows the function calls and prints answers
; File to test CS 378 Assignment 1:  by a student, Andrew Smith

; (load-file "asg1.clj")
; user=> (stdev lstnum)
; 10.2384317531809

(defn qstr [x]
  (if (and (cons? x) (= (first x) 'quote))
      (str " '" (second x))
      (str " " x)))

(defn qlst [x]
  (if (cons? x)
      (eval (cons 'str (cons "(" (cons (str (first x))
              (append (map qstr (rest x)) (list ")"))))))
      x))

(defn runpr [form]
  (let [answer (eval form)]
    (println (qlst form) "   =   " answer) ))

(defn runprlist [lst]
  (doseq [x lst] (runpr x)))

; (runprlist summations)
(def summations '(
    (sum '())
    (sum '(42))
    (sum '(1 2 3 4 5))
    (sum '(1 -1 2 -2))
    (sumtr '())
    (sumtr '(42))
    (sumtr '(1 2 3 4 5))
    (sumtr '(1 -1 2 -2))
    (sumr '())
    (sumr '(42))
    (sumr '(1 2 3 4 5))
    (sumr '(1 -1 2 -2))
))

; (runprlist square-sums)
(def square-sums
'(
    (sumsq  '())
    (sumsq  '(1))
    (sumsq  '(-1))
    (sumsq  '(3))
    (sumsq  '(3 4))
    (sumsq  '(1 -2 3 4 -5))
    (sumsqtr  '())
    (sumsqtr  '(1))
    (sumsqtr  '(-1))
    (sumsqtr  '(3))
    (sumsqtr  '(3 4))
    (sumsqtr  '(1 -2 3 4 -5))
    (sumsqmr  '())
    (sumsqmr  '(1))
    (sumsqmr  '(-1))
    (sumsqmr  '(3))
    (sumsqmr  '(3 4))
    (sumsqmr  '(1 -2 3 4 -5))
))

; (runprlist unions)
(def unions '(
  (union '() '())
  (union '(1) '())
  (union '() '(1))
  (union '(1 2) '())
  (union '(1) '(2))
  (union '(1 2) '(3))
  (union '(1) '(2 3))
  (union '(1 4) '(2 3))
))

; (runprlist set-differences)
(def set-differences '(
  (set-difference '() '())
  (set-difference '(1) '())
  (set-difference '() '(1))
  (set-difference '(1) '(1))
  (set-difference '(1) '(2))
  (set-difference '(1 2 3) '(1 3))
  (set-difference '(1 2 3) '(2 4 5))
))

; (runprlist binomials)
(def binomials '(
 (binomial 0)
 (binomial 1)
 (binomial 2)
 (binomial 3)
 (binomial 4)
))

; (runprlist maxbts)
(def maxbts '(
  (maxbt 42)
  (maxbt '(42))
  (maxbt '(37 42))
  (maxbt '(37 39 42))
  (maxbt '(42 ()))
  (maxbt '(() 42 ()))
  (maxbt '(() 42 a b 37))
  (maxbt '((1 pie 7) (-3 2 eggs) (((foo (((42))))))))
))

; (runprlist varss)
(def varss '(
  (vars 42)
  (vars 'a)
  (vars '(= 42 a))
  (vars '(= (+ b c) a))
  (vars '(= (+ b c) (Math/sqrt a b)))
  (vars '(= f (* m a)))
))

; (runprlist occurss)
(def occurss '(
  (occurs 'x '())
  (occurs 'x 'x)
  (occurs 'x '(x))
  (occurs 42 '(42))
  (occurs 'x '(42 x))
  (occurs 'x '(= x 42))
  (occurs 'x '(= (+ x y 42)))
  (occurs 'z '(= (+ x y 42)))
  (occurs 'm '(= f (* m a)))
))

; (runprlist evals)
(def evals '(
  (myeval 42)
  (myeval '(- 42))
  (myeval '(+ 11 31))
  (myeval '(- 49 7))
  (myeval '(* 21 2))
  (myeval '(/ 3318 79))
  (myeval '(expt 2 7))
  (myeval '(- 11 (- 31)))
  (myeval '(+ 3 (* 5 7)))
))

; (runprlist eval-bindings)
(def eval-bindings '(
  (myevalb 42 '())
  (myevalb 'a '((a 42)))
  (myevalb '(+ a 31) '((a 11)))
  (myevalb '(+ 11 a) '((a 31)))
  (myevalb '(+ a b) '((a 11) (b 31)))
  (myevalb '(- a) '((b 31) (a -42)))
  (myevalb '(+ 7 (* 5 b)) '((b 7)))
))

; (runprlist tojavas)
(def tojavas '(
  (tojava 'x)
  (tojava 42)
  (tojava '(- x))
  (tojava '(- 42))
  (tojava '(= x y))
  (tojava '(= x 42))
  (tojava '(= x (- 42)))
  (tojava '(= x (+ y z)))
  (tojava '(= x (- y z)))
  (tojava '(= x (* y z)))
  (tojava '(= x (/ y z)))
  (tojava '(= x (+ a (- b))))
  (tojava '(= x (* (+ a b) c)))
  (tojava '(= x (* (+ a b) (- (+ (- c) d)))))
  (tojava '(sin 42))
  (tojava '(sin (cos (tan 42))))
  (tojava '(= x (* (+ a b) (sin (/ c d)))))
  (tojava '(= x (* (+ a b) (- (sin (/ c (atan d)))))))
))

(defn runall []
 (runprlist summations)
 (runprlist square-sums)
 (runprlist unions)
 (runprlist set-differences)
 (runprlist binomials)
 (runprlist maxbts)
 (runprlist varss)
 (runprlist occurss)
 (runprlist evals)
 (runprlist eval-bindings)
 (runprlist tojavas)
)
