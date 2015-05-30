(ns madoka-population.state)

;; Could be (def keyword->symbol (comp symbol #(subs % 1) str)), which
;; is pretty cool but can't have a precondition. (This is where static
;; typing could come in handy.
(defn keyword->symbol
  "Converts a keyword into a symbol via a string."
  [kw]
  {:pre [(keyword? kw)]}
  (-> kw
      str
      (subs 1)
      symbol))

(defn convert-to-symbol
  "Converts a keyword, string, or symbol into a symbol."
  [key]
  (condp = (type key)
    clojure.lang.Keyword (keyword->symbol key)
    java.lang.String (symbol key)
    clojure.lang.Symbol key))

;; See also my extensive notes in the design doc on how the current version
;; came to be.
(defn add-vars-to-ns
  "Adds bindings in binding-map as bound vars in the current namespace."
  [binding-map]
  (eval
   (cons 'do
         (map
          #(list 'def (convert-to-symbol (first %)) (second %))
          binding-map))))

(defmacro with-bindings-from
  "Takes a map of keywords to values and executes forms in a scope with
  symbols (that have the same names as the keywords) bound to those values."
  [bindings & forms]
  {:pre [(map? bindings) (not-empty bindings)]}
  `(let [~@(flatten (map #(vector (keyword->symbol (% 0)) (% 1)) bindings))]
     ~@forms))
