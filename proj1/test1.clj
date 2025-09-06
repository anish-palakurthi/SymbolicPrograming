; File to test CS 378 Assignment 1:  by a student, Andrew Smith

; (load-file "asg1.clj")
; (run-tests)

; to run the tests one at a time, you can:
; (load-file "cs378/onetest.clj") after loading this file
; (one-test 'test-summations)    ; etc.  Note the quote

; user=> (stdev lstnum)
; 10.2384317531809

(use 'clojure.test)

(deftest test-summations
  (doseq [f [sum sumtr sumr]]
    (is (= 0 (f '())))
    (is (= 42 (f '(42))))
    (is (= 15 (f '(1 2 3 4 5))))
    (is (= 0 (f '(1 -1 2 -2))))))

(deftest test-square-sums
  (doseq [f [sumsq sumsqtr sumsqmr]]
    (is (= 0 (f '())))
    (is (= 1 (f '(1))))
    (is (= 1 (f '(-1))))
    (is (= 9 (f '(3))))
    (is (= 25 (f '(3 4))))
    (is (= 55 (f '(1 -2 3 4 -5))))))

(defn- run-set-test
  ([func expected arg]
   (is (= (set expected) (set (func arg)))))
  ([func expected arg1 arg2]
   (is (= (set expected) (set (func arg1 arg2))))))

(deftest test-union
  (run-set-test union '() '() '())
  (run-set-test union '(1) '(1) '())
  (run-set-test union '(1) '() '(1))
  (run-set-test union '(1 2) '(1 2) '())
  (run-set-test union '(1 2) '() '(1 2))
  (run-set-test union '(1 2) '(1) '(2))
  (run-set-test union '(1 2 3) '(1 2) '(3))
  (run-set-test union '(1 2 3) '(1) '(2 3))
  (run-set-test union '(1 2 3 4) '(1 4) '(2 3)))

(deftest test-set-difference
  (run-set-test set-difference '() '() '())
  (run-set-test set-difference '(1) '(1) '())
  (run-set-test set-difference '() '() '(1))
  (run-set-test set-difference '() '(1) '(1))
  (run-set-test set-difference '(1) '(1) '(2))
  (run-set-test set-difference '(2) '(1 2 3) '(1 3))
  (run-set-test set-difference '(1 3) '(1 2 3) '(2 4 5)))

(deftest test-binomial
  (is (= '(    1    ) (binomial 0)))
  (is (= '(   1 1   ) (binomial 1)))
  (is (= '(  1 2 1  ) (binomial 2)))
  (is (= '( 1 3 3 1 ) (binomial 3)))
  (is (= '(1 4 6 4 1) (binomial 4))))

(defn- run-maxbt-test [tree]
  (is (= 42 (maxbt tree))))

(deftest test-binary-tree-max
  (run-maxbt-test 42)
  (run-maxbt-test '(42))
  (run-maxbt-test '(37 42))
  (run-maxbt-test '(37 39 42))
  (run-maxbt-test '(42 ()))
  (run-maxbt-test '(() 42 ()))
  (run-maxbt-test '(() 42 a b 37))
  (run-maxbt-test '((1 pie 7) (-3 2 eggs) (((foo (((42)))))))))

(deftest test-fn-vars
  (run-set-test vars '() 42)
  (run-set-test vars '(a) 'a)
  (run-set-test vars '(a) '(= 42 a))
  (run-set-test vars '(a b c) '(= (+ b c) a))
  (run-set-test vars '(a b c) '(= (+ b c) (Math/sqrt a b)))
  (run-set-test vars '(a f m) '(= f (* m a))))

(deftest test-occurs
  (is (not (occurs 'x '())))
  (is (occurs 'x 'x))
  (is (occurs 'x '(x)))
  (is (occurs 42 '(42)))
  (is (occurs 'x '(42 x)))
  (is (occurs 'x '(= x 42)))
  (is (occurs 'x '(= (+ x y 42))))
  (is (not (occurs 'z '(= (+ x y 42)))))
  (is (occurs 'm '(= f (* m a)))))

(defn- run-unary-test [func expected expr]
  (is (= expected (func expr))))

(deftest test-eval
  (run-unary-test myeval 42 42)
  (run-unary-test myeval -42 '(- 42))
  (run-unary-test myeval 42 '(+ 11 31))
  (run-unary-test myeval 42 '(- 49 7))
  (run-unary-test myeval 42 '(* 21 2))
  (run-unary-test myeval 42 '(/ 3318 79))
  (run-unary-test myeval 128.0 '(expt 2 7))
  (run-unary-test myeval 42 '(- 11 (- 31)))
  (run-unary-test myeval 38 '(+ 3 (* 5 7))))

(defn- run-evalb-test [expr bindings]
  (is (= 42 (myevalb expr bindings))))

(deftest test-eval-bindings
  (run-evalb-test 42 '())
  (run-evalb-test 'a '((a 42)))
  (run-evalb-test '(+ a 31) '((a 11)))
  (run-evalb-test '(+ 11 a) '((a 31)))
  (run-evalb-test '(+ a b) '((a 11) (b 31)))
  (run-evalb-test '(- a) '((b 31) (a -42)))
  (run-evalb-test '(+ 7 (* 5 b)) '((b 7))))

(deftest test-tojava
  (run-unary-test tojava "x;" 'x)
  (run-unary-test tojava "42;" 42)
  (run-unary-test tojava "(-x);" '(- x))
  (run-unary-test tojava "(-42);" '(- 42))
  (run-unary-test tojava "x=y;" '(= x y))
  (run-unary-test tojava "x=42;" '(= x 42))
  (run-unary-test tojava "x=(-42);" '(= x (- 42)))
  (run-unary-test tojava "x=y+z;" '(= x (+ y z)))
  (run-unary-test tojava "x=y-z;" '(= x (- y z)))
  (run-unary-test tojava "x=y*z;" '(= x (* y z)))
  (run-unary-test tojava "x=y/z;" '(= x (/ y z)))
;  (run-unary-test tojava "x=(y=z);" '(= x (= y z)))
  (run-unary-test tojava "x=a+(-b);" '(= x (+ a (- b))))
  (run-unary-test tojava "x=(a+b)*c;" '(= x (* (+ a b) c)))
  (run-unary-test tojava "x=(a+b)*(-((-c)+d));" '(= x (* (+ a b) (- (+ (- c) d)))))
  (run-unary-test tojava "Math.sin(42);" '(sin 42))
  (run-unary-test tojava "Math.sin(Math.cos(Math.tan(42)));" '(sin (cos (tan 42))))
  (run-unary-test tojava "x=(a+b)*Math.sin(c/d);" '(= x (* (+ a b) (sin (/ c d)))))
  (run-unary-test tojava "x=(a+b)*(-Math.sin(c/Math.atan(d)));" '(= x (* (+ a b) (- (sin (/ c (atan d))))))))
