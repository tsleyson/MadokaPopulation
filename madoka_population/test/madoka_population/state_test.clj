(ns 'madoka-population.state-test
  (:require [clojure.test :refer :all]
            [madoka-population.state :as state]))

;;;; See http://stackoverflow.com/a/27343708/3376926
;; (defmacro with-ns
;;   "Evaluates body in another namespace. ns is a namespace object or a
;;   symbol."
;;   [ns & body]
;;   `(binding [*ns* (the-ns ~ns)]
;;      ~@(map (fn [form] `(eval '~form)) body)))

;; (defmacro with-temp-ns
;;   "Evaluates body in an anonymous namespace which is removed once the
;;   body is evaluated. The temporary namespace refers clojure.core."
;;   [& body]
;;   `(try
;;      (create-ns 'sym#)
;;      (let [result# (with-ns 'sym#
;;                      (clojure.core/refer-clojure)
;;                      ~@body)]
;;        result#)
;;      (finally (remove-ns 'sym#))))

;; (defmacro vars-bound?
;;   "Returns truthy if all given vars are bound in the current
;;   namespace, else returns nil."
;;   [& vars]
;;   `(and ~@(map #(list 'get-possibly-unbound-var #'%) vars)))

;; (defn in-new-namespace
;;   [test]
;;   (in-ns 'test)
;;   (test)
;;   (in-ns 'madoka-population.state-test))

;; (use-fixtures :each in-new-namespace)

(deftest test-adding-vars-to-namespace
  (testing "The bindings from a literal map get created."
    (with-temp-ns
      (state/add-vars-to-ns '{a 1, b 2})
      (is (vars-bound? a b))))
  (testing "The bindings from a map in a var get created."
    (with-temp-ns
      (let [m {c 3, d 4}]
        (state/add-vars-to-ns m)
        (is (vars-bound? c d)))))
  (testing "The bindings from a read in config file get created."
    (with-temp-ns
      (-> config-file
          slurp
          clojure.edn/read-string
          state/add-vars-to-ns)
      (is (vars-bound? incubator-count incubator-mean-success world-size
                       starting-magical-girls starting-witches
                       turns-per-day)))))
