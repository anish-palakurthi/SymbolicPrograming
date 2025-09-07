;; Anish Palakurthi - ap58766 - Symbolic Programming
;; CS 378 — Assignment 1 
; (load-file "cs378.clj")
; (load-file "asg1.clj")
; (load-file "proj1/test1.clj")
; (load-file "proj1/onetest.clj")
; (run-tests)
; (one-test 'test-summations)
; (runall)
; (load-file "cs378.clj")(load-file "proj1/asg1.clj")(load-file "proj1/test1.clj")(load-file "proj1/onetest.clj")(load-file "proj1/test1b.clj")

(defn sum [lst]
  ;; simple recursion 
  (if (empty? lst)
    0
    (+ (first lst) (sum (rest lst)))))


(defn sumtr [lst]
  (loop [l lst
         acc 0]
    (if (empty? l)
      acc
      (recur (rest l) (+ acc (first l))))))

(defn sumr [lst]
  ;; reduce with 0 seed is safe on empty lists
  (reduce + 0 lst))

(defn sumsq [lst]
  (if (empty? lst)
    0
    (+ (* (first lst) (first lst)) (sumsq (rest lst)))))

(defn sumsqtr [lst]
  (loop [l lst
         acc 0]
    (if (empty? l)
      acc
      (recur (rest l) (+ acc (square (first l)))))))

(defn sumsqmr [lst]
  (reduce + (map square lst)))

(defn stdev [lst]
  (let [n (length lst)
        mean (/ (sum lst) n)
        mean-square (/ (sumsq lst) n)
        variance (- mean-square (square mean))]
    (sqrt variance)))

(def lstnum '(76 85 71 83 84 89 96 84 98 97 75 85 92 64 89 87 90 65 100))

;; ---------- List-as-set utilities ----------
;; Note: these treat lists as mathematical sets (no ordering guarantees).

(defn intersection [x y]
  (if (empty? x)
    '()
    (if (some #(= (first x) %) y)
      (cons (first x)
            (intersection (rest x) y))
      (intersection (rest x) y))))

(defn union [lsta lstb]
  (letfn [(helper [a b acc]
            (cond
              (empty? a) (reduce #(if (some #{%2} %1) %1 (cons %2 %1)) acc b)
              (some #(= (first a) %) acc) (helper (rest a) b acc)
              :else (helper (rest a) b (cons (first a) acc))))]
    (helper lsta lstb '())))


(defn set-difference [lsta lstb]
  (letfn [(helper [a b acc]
            (cond
              (empty? a) acc
              (some #(= (first a) %) b) (helper (rest a) b acc)
              :else (helper (rest a) b (cons (first a) acc))))]
    (helper lsta lstb '())))

;; ---------- Pascal/binomial row ----------

(defn binomial [n]
  ;; build row n via iterate with pairwise sums
  (letfn [(next-row [row]
            (let [mids (map + row (rest row))]
              (concat '(1) mids '(1))))]
    (nth (iterate next-row '(1)) n)))

;; ---------- Tree helpers ----------

(defn maxbt [tree]
  ;; walk any nested list/number structure
  (letfn [(walk [t cur]
            (cond
              (number? t) (max cur t)
              (sequential? t) (reduce (fn [m sub] (walk sub m))
                                      cur t)
              :else cur))]
    (walk tree Long/MIN_VALUE)))


(defn vars [expr]
  ;; Return all variable symbols: symbols with NO namespace and not in the operator set.
  (let [ops #{'+ '- '* '/ 'expt 'sqrt 'exp 'log '=}]
    (letfn [(collect [node acc]
              (cond
                (number? node) acc
                (symbol? node)
                (if (or (ops node) (namespace node))
                  acc
                  (conj acc node))
                (sequential? node)
                ;; first is operator; rest are operands—walk them
                (reduce (fn [a x] (collect x a)) acc (rest node))
                :else acc))]
      (let [s (collect expr #{})]
        (if (empty? s) '() (seq s))))))

(defn occurs [item tree]
  ;; membership test anywhere in a nested structure
  (boolean (some #(= % item) (tree-seq coll? seq tree))))

;; ---------- Evaluators ----------

(defn myeval [tree]
  (letfn [(E [node]
            (cond
              (number? node) node
              (list? node)
              (let [[op & args] node]
                (case op
                  + (reduce + (map E args))
                  * (reduce * (map E args))
                  - (case (count args)
                      1 (- (E (first args)))
                      2 (- (E (first args)) (E (second args)))
                      (throw (Exception. "arity for - must be 1 or 2")))
                  / (let [[a b] (map E args)]
                      (/ a b))
                  expt (let [[a b] (map E args)]
                         (Math/pow a b))
                  sqrt (Math/sqrt (E (first args)))
                  exp  (Math/exp  (E (first args)))
                  log  (Math/log  (E (first args)))
                  (throw (Exception. (str "unsupported operation: " op)))))
              :else (throw (Exception. (str "invalid expression: " node)))))]
    (E tree)))

(defn myevalb [tree bindings]
  (letfn [(lookup [v]
            (let [p (assocl v bindings)]
              (if p (second p)
                  (throw (Exception. (str "Unbound variable: " v))))))
          (E [node]
            (cond
              (number? node) node
              (symbol? node) (lookup node)
              (list? node)
              (let [[op & args] node]
                (case op
                  + (reduce + (map E args))
                  * (reduce * (map E args))
                  - (case (count args)
                      1 (- (E (first args)))
                      2 (- (E (first args)) (E (second args)))
                      (throw (Exception. "arity for - must be 1 or 2")))
                  / (let [[a b] (map E args)] (/ a b))
                  expt (let [[a b] (map E args)] (Math/pow a b))
                  sqrt (Math/sqrt (E (first args)))
                  exp  (Math/exp  (E (first args)))
                  log  (Math/log  (E (first args)))
                  (throw (Exception. (str "unsupported operation: " op)))))
              :else (throw (Exception. (str "invalid expression: " node)))))]
    (E tree)))

;; ---------- Java stringifier ----------
;; Requires op/lhs/rhs helpers from cs378.clj, but the formatting logic is new.

(defn tojava [tree]
  (let [prec {= 1, + 5, - 5, * 6, / 6}
        funs #{'sqrt 'exp 'log 'sin 'cos 'tan}
        paren-if (fn [need? s] (if need? (str "(" s ")") s))]
    (letfn [(J [node ctx]
              (cond
                (list? node)
                (let [o (op node)
                      L (lhs node)
                      R (rhs node)]
                  (cond
                    (= o '=)
                    (str (J L 1) "=" (J R 1))

                    (and (= o '-) (nil? R))              ; unary minus
                    (str "(-" (J L 7) ")")

                    (or (= o '+) (= o '-))
                    (let [s (str (J L 5) (name o) (J R 5))]
                      (paren-if (> ctx 5) s))

                    (or (= o '*) (= o '/))
                    (let [s (str (J L 6) (name o) (J R 6))]
                      (paren-if (> ctx 6) s))

                    (funs o)                             ; function call: no extra parens on arg
                    (str "Math." (name o) "(" (J L 0) ")")

                    :else                                ; unknown op -> treat like Math.<op>(args)
                    (str "Math." (name o) "(" (J L 0)
                         (when R (str "," (J R 0))) ")")))
                :else
                (str node)))]
      (str (J tree 0) ";"))))
