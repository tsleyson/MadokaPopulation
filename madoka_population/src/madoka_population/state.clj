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

;; See http://stackoverflow.com/a/29759517/3376926. I modified that
;; answer to do a def instead of a defrecord.
;; See also my extensive notes in the design doc on how the current version
;; came to be.
(defmacro add-vars-to-ns
  "Takes a map of keys to values and binds those values to symbols in
  the current namespace whose names are the keys under conversion by
  convert-to-symbol."
  [binding-map]
  `(do ~@(map #(list 'def (convert-to-symbol (first %)) (second %))
              binding-map)))

(defmacro with-bindings-from
  "Takes a map of keywords to values and executes forms in a scope with
  symbols (that have the same names as the keywords) bound to those values."
  [bindings & forms]
  {:pre [(map? bindings) (not-empty bindings)]}
  `(let [~@(flatten (map #(vector (keyword->symbol (% 0)) (% 1)) bindings))]
     ~@forms))
