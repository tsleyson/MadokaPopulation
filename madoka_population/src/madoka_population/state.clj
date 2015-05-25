(ns madoka-population.state)

;; Could be (def keyword->symbol (comp symbol #(subs % 1) str)), which
;; is pretty cool but can't have a precondition.
(defn keyword->symbol
  "Converts a keyword into a symbol via a string."
  [kw]
  {:pre [(keyword? kw)]}
  (-> kw
      str
      (subs 1)
      symbol))

;; See http://stackoverflow.com/a/29759517/3376926. I modified that
;; answer to do a def instead of a defrecord.
(defmacro add-vars-to-ns
  "Takes a map of symbols to values and binds those values to those
  symbols in the current namespace."
  [binding-map]
  `(do ~@(map #(list 'def (first %) (second %)) binding-map)))

(defmacro with-bindings-from
  "Takes a map of keywords to values and executes forms in a scope with
  symbols (that have the same names as the keywords) bound to those values."
  [bindings & forms]
  {:pre [(map? bindings) (not-empty bindings)]}
  `(let [~@(flatten (map #(vector (keyword->symbol (% 0)) (% 1)) bindings))]
     ~@forms))
