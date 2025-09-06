;; CS 378 — Assignment 1 (no-namespace version)
; (load-file "cs378.clj")
; (load-file "asg1_ap58766.clj")
; (load-file "proj1/test1.clj")
; (load-file "proj1/onetest.clj")
; (run-tests)
; (one-test 'test-summations)
; (runall)
; (load-file "test1b.clj")
; (one-test `test-tojava)
; (load-file "cs378.clj")(load-file "proj1/asg1_ap58766.clj")(load-file "proj1/test1.clj")(load-file "proj1/onetest.clj")(load-file "test1b.clj")

;; ---------- Provided helper shapes ----------
(defn op  [expr] (when (seq? expr) (first expr)))
(defn lhs [expr] (when (seq? expr) (second expr)))
(defn rhs [expr] (when (seq? expr) (nth expr 2 nil)))

;; association list lookup: returns the PAIR (k v) or nil
(defn assocl [k alist]
  (some #(when (= (first %) k) %) alist))

;; ---------- 1) sum / sumtr / sumr ----------
(defn sum [lst]
  (if (empty? lst) 0 (+ (first lst) (sum (rest lst)))))

(defn sumtr
  ([lst] (sumtr lst 0))
  ([lst acc]
   (if (empty? lst) acc (recur (rest lst) (+ acc (first lst))))))

(defn sumr [lst]
  (reduce + 0 lst))

;; ---------- 2) sumsq / sumsqtr / sumsqmr ----------
(defn sumsq [lst]
  (if (empty? lst) 0 (+ (* (first lst) (first lst)) (sumsq (rest lst)))))

(defn sumsqtr
  ([lst] (sumsqtr lst 0))
  ([lst acc]
   (if (empty? lst)
     acc
     (let [x (first lst)] (recur (rest lst) (+ acc (* x x)))))))

(defn sumsqmr [lst]
  (reduce + 0 (map #(* % %) lst)))

;; ---------- 3) stdev ----------
(def lstnum '(76 85 71 83 84 89 96 84 98 97 75 85 92 64 89 87 90 65 100))
(defn stdev [lst]
  (let [n (count lst)]
    (if (pos? n)
      (let [s (double (sum lst))
            s2 (double (sumsq lst))
            mean (/ s n)
            var  (- (/ s2 n) (* mean mean))]
        (Math/sqrt var))
      0.0)))

;; ---------- 4) union / set-difference (tail-recursive) ----------
(defn member? [x xs] (some #(= x %) xs))

(defn union
  ([a b] (union a b '()))
  ([a b acc]
   (cond
     (and (empty? a) (empty? b)) (reverse acc)
     (empty? a) (let [x (first b)]
                  (recur a (rest b) (if (member? x acc) acc (conj acc x))))
     :else      (let [x (first a)]
                  (recur (rest a) b (if (member? x acc) acc (conj acc x)))))))

(defn set-difference
  ([a b] (set-difference a b '()))
  ([a b acc]
   (if (empty? a)
     (reverse acc)
     (let [x (first a)]
       (recur (rest a) b (if (member? x b) acc (conj acc x)))))))

;; ---------- 5) binomial via Pascal ----------
(defn next-row [row]
  (let [pairs (partition 2 1 row)
        mids  (map (fn [[u v]] (+ u v)) pairs)]
    (concat '(1) mids '(1))))

(defn binomial [n]
  (loop [k 0 row '(1)]
    (if (= k n) row (recur (inc k) (next-row row)))))

;; ---------- 6) maxbt ----------
(def ^:private -inf -1000000)
(defn maxbt [tree]
  (cond
    (number? tree) tree
    (and (seq? tree) (seq tree))
    (max (maxbt (first tree)) (maxbt (rest tree)))
    :else -inf))

;; ---------- 7) vars ----------
(defn vars [expr]
  (cond
    (number? expr) '()
    (symbol? expr) (list expr)
    (seq? expr)    (union (vars (lhs expr)) (vars (rhs expr)))
    :else          '()))

;; ---------- 8) occurs ----------
(defn occurs [item tree]
  (cond
    (seq? tree) (or (occurs item (first tree))
                    (occurs item (rest tree)))
    :else (= item tree)))

;; ---------- 9) myeval (unary/binary -, +, -, *, /, expt, sqrt, exp, log) ----------
(defn- eval-op [o args]
  (cond
    (= o '+)   (apply + args)
    (= o '*)   (apply * args)
    (= o '/)   (apply / args)
    (= o '-)   (if (= 1 (count args))
                 (- (first args))
                 (- (first args) (second args)))
    (= o 'expt) (Math/pow (double (first args)) (double (second args)))
    (= o 'sqrt) (Math/sqrt (double (first args)))
    (= o 'exp)  (Math/exp  (double (first args)))
    (= o 'log)  (Math/log  (double (first args)))
    :else (throw (ex-info "Unknown operator" {:op o :args args}))))

(defn myeval [tree]
  (cond
    (number? tree) tree
    (seq? tree)
    (let [o (op tree) l (lhs tree) r (rhs tree)]
      (cond
        (= o '-)   (if (some? r)
                     (eval-op '- [(myeval l) (myeval r)])
                     (eval-op '- [(myeval l)]))
        (#{'+ '* '/ 'expt} o) (eval-op o [(myeval l) (myeval r)])
        (#{'sqrt 'exp 'log} o) (eval-op o [(myeval l)])
        :else (throw (ex-info "Unsupported op" {:op o}))))
    :else (throw (ex-info "Bad node" {:node tree}))))

;; ---------- 10) myevalb with bindings ----------
(defn myevalb [tree bindings]
  (cond
    (number? tree) tree
    (symbol? tree) (let [p (assocl tree bindings)]
                     (if p (second p) (throw (ex-info "Unbound variable" {:var tree}))))
    (seq? tree)
    (let [o (op tree) l (lhs tree) r (rhs tree)]
      (cond
        (= o '-)   (if (some? r)
                     (eval-op '- [(myevalb l bindings) (myevalb r bindings)])
                     (eval-op '- [(myevalb l bindings)]))
        (#{'+ '* '/ 'expt} o) (eval-op o [(myevalb l bindings) (myevalb r bindings)])
        (#{'sqrt 'exp 'log} o) (eval-op o [(myevalb l bindings)])
        :else (throw (ex-info "Unsupported op" {:op o}))))
    :else (throw (ex-info "Bad node" {:node tree}))))

; The 'tojava' function converts an expression tree into a Java statement string.
; It processes the tree recursively and applies the correct precedence rules.
(defn tojava [tree]
  ; The 'process-tree' helper function handles the recursive traversal and formatting of the expression tree.
  ; It accepts the current tree and an operator precedence level to manage parentheses.
  (letfn [(process-tree [node precedence]
            (cond
              ; If the node is a cons (a list), evaluate the operator and operands.
              (cons? node)
              (let [operator (op node)
                    left-side (lhs node)
                    right-side (rhs node)
                    ; Determine the precedence based on the operator type.
                    operator-precedence (case operator
                                          = 1
                                          + 5
                                          - 5
                                          * 6
                                          / 6
                                          7)] ; Precedence for function arguments and unary minus
                (case operator
                  ; Handle the assignment operator
                  =
                  (str (process-tree left-side 1) "=" (process-tree right-side 1))                  
                  ; Handle addition and subtraction operators
                  (+ -)
                  (if right-side
                    (let [left-str (process-tree left-side 5)
                          right-str (process-tree right-side 5)]
                      (if (> precedence 5)
                        (str "(" left-str operator right-str ")")
                        (str left-str operator right-str)))
                    ; Handle the unary minus operation
                    (str "(-" (process-tree left-side 7) ")"))                  
                  ; Handle multiplication and division operators
                  (* /)
                  (let [left-str (process-tree left-side 6)
                        right-str (process-tree right-side 6)]
                    (if (and right-side (> precedence 6))
                      (str "(" left-str operator right-str ")")
                      (str left-str operator right-str)))
                  ; Handle mathematical functions like sin, cos, etc.
                  (let [argument-str (process-tree left-side 6)]
                    (str "Math." operator "(" argument-str ")"))))
              ; Base case: if the node is not a cons, convert it to a string
              :else (str node)))]
    ; Start processing with precedence 0 and add a semicolon to the final result.
    (str (process-tree tree 0) ";")))
;; ---------- Quick sanity checks (ignored by loader) ----------
(comment
  (sum '(1 2 3 4))                          ; 10
  (sumtr '(1 2 3 4))                        ; 10
  (sumr '(1 2 3 4))                         ; 10

  (sumsq '(1 2 3))                          ; 14
  (sumsqtr '(1 2 3))                        ; 14
  (sumsqmr '(1 2 3))                        ; 14

  (stdev lstnum)

  (union '(a b c) '(b d e))                 ; order may vary
  (set-difference '(a b c) '(b d e))        ; => (a c)

  (binomial 5)                              ; (1 5 10 10 5 1)

  (maxbt '((1 pie 7) (-3 2 eggs) (((foo (((8))) ))))) ; 8

  (vars '(= f (* m a)))                     ; (f m a)
  (occurs 'm '(= f (* m a)))                ; true

  (myeval '(+ 3 (* 5 7)))                   ; 38
  (myevalb '(+ 3 (* 5 b)) '((b 7)))         ; 38

  (tojava '(= x (* (+ a b) c)))             ; "x=(a+b)*c;"
  (tojava '(- x))                           ; "(-x);"
  (tojava '(= y (expt x 3)))                ; "y=Math.pow(x,3);"
  )
