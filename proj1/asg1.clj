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
  ;; simple structural recursion with destructuring
  (if-let [[h & t] (seq lst)]
    (+ h (sum t))
    0))

(defn sumtr [lst]
  ;; explicit loop/recur accumulator form
  (loop [xs lst, acc 0]
    (if (seq xs)
      (recur (rest xs) (+ acc (first xs)))
      acc)))

(defn sumr [lst]
  ;; reduce with 0 seed is safe on empty lists
  (reduce + 0 lst))

(defn sumsq [lst]
  (if-let [[h & t] (seq lst)]
    (+ (* h h) (sumsq t))
    0))

(defn sumsqtr [lst]
  (loop [xs lst, acc 0]
    (if-let [[h & t] (seq xs)]
      (recur t (+ acc (* h h)))
      acc)))

(defn sumsqmr [lst]
  (reduce + 0 (map #(* % %) lst)))

(defn stdev [lst]
  ;; population stdev; coerce counts to double to avoid ratio surprises
  (let [n (count lst)]
    (when (zero? n) (throw (Exception. "stdev of empty list")))
    (let [n' (double n)
          mean (/ (reduce + lst) n')
          mean2 (/ (reduce + (map #(* % %) lst)) n')
          var (- mean2 (* mean mean))]
      (Math/sqrt var))))

(def lstnum '(76 85 71 83 84 89 96 84 98 97 75 85 92 64 89 87 90 65 100))

;; ---------- List-as-set utilities ----------
;; Note: these treat lists as mathematical sets (no ordering guarantees).

(defn intersection [x y]
  (let [ys (set y)]
    (loop [xs x, acc '(), seen #{}]
      (if (seq xs)
        (let [a (first xs)]
          (if (and (ys a) (not (seen a)))
            (recur (rest xs) (cons a acc) (conj seen a))
            (recur (rest xs) acc seen)))
        (reverse acc)))))

(defn union [lsta lstb]
  (let [seen-step (fn [state v]
                    (if ((:seen state) v)
                      state
                      (-> state
                          (update :out conj v)
                          (update :seen conj v))))]
    (let [{:keys [out]}
          (reduce seen-step {:out [] :seen #{}} (concat lsta lstb))]
      (if (empty? out)
        '()
        (seq out)))))


(defn set-difference [lsta lstb]
  (let [bys (set lstb)]
    (loop [xs lsta, acc '(), seen #{}]
      (if (seq xs)
        (let [a (first xs)]
          (if (or (bys a) (seen a))
            (recur (rest xs) acc seen)
            (recur (rest xs) (cons a acc) (conj seen a))))
        (reverse acc)))))

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
