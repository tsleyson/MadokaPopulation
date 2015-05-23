(ns madoka-population.state
  (:require [madoka-population.events :as events]))

;; See http://stackoverflow.com/a/29759517/3376926. I modified that
;; answer to do a def instead of a defrecord.
(defmacro add-vars
  "Takes a map of symbols to values and binds those values to those
  symbols in the current namespace."
  [binding-map]
  `(do ~@(map #(list 'def (first %) (second %)) binding-map)))

(defmacro with-bindings-from
  "Takes a map of symbols to values and executes forms in a scope with
  those symbols bound to those values."
  [bindings & forms]
  `(let [~@(flatten (map identity bindings))]
     ~@forms))
