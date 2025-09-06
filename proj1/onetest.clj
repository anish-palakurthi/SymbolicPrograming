; onetest.clj           G. Novak        20 Jan 22

; Example:   (one-test 'test-summations)

(defn test-vars-b
  "Groups vars by their namespace and runs test-vars on them with
   appropriate fixtures applied."
  {:added "1.6"}
  [vars z]
  (doseq [[ns vars] (group-by (comp :ns meta) vars)]
    (let [once-fixture-fn (join-fixtures (::once-fixtures (meta ns)))
          each-fixture-fn (join-fixtures (::each-fixtures (meta ns)))]
      (once-fixture-fn
       (fn []
         (doseq [v vars]
           (when (and (:test (meta v)) (= v (resolve z)))
             (each-fixture-fn (fn [] (test-var v))))))))))

(defn test-all-vars-b
  "Calls test-vars on every var interned in the namespace, with fixtures."
  {:added "1.1"}
  [ns z]
  (test-vars-b (vals (ns-interns ns)) z))

(defn test-ns-b
  "If the namespace defines a function named test-ns-hook, calls that.
  Otherwise, calls test-all-vars on the namespace.  'ns' is a
  namespace object or a symbol.
  Internally binds *report-counters* to a ref initialized to
  *initial-report-counters*.  Returns the final, dereferenced state of
  *report-counters*."
  {:added "1.1"}
  [ns z]
  (binding [*report-counters* (ref *initial-report-counters*)]
    (let [ns-obj (the-ns ns)]
      (do-report {:type :begin-test-ns, :ns ns-obj})
      ;; If the namespace has a test-ns-hook function, call that:
      (if-let [v (find-var (symbol (str (ns-name ns-obj)) "test-ns-hook"))]
	((var-get v))
        ;; Otherwise, just test every var in the namespace.
        (test-all-vars-b ns-obj z))
      (do-report {:type :end-test-ns, :ns ns-obj}))
    @*report-counters*))

(defn one-test [test] (test-ns-b *ns* test))
