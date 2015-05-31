(ns madoka-population.core-test
  (:require [clojure.test :refer :all]
            [madoka-population.core :as core]))

;; Test parameter definitions
(def world-size 200)
(def incubator-count 10)
(def incubator-mean-success 0.5)
(def starting-magical-girls 5)
(def starting-witches 5)
(def bundle ;;die
  )

(deftest test-new-bundle
  (let [test-bundle ((core/setup-function
                      (core/get-config nil)
                      :testing true))]
    (testing "The state gets initialized correctly."
      (are [expected actual] (= expected (count actual))
           starting-magical-girls (:magical-girls test-bundle)
           starting-witches (:witches test-bundle)
           incubator-count (:incubators test-bundle))
      (is ((:within-world? test-bundle) (/ world-size 2)))
      (is (zero? (:turns test-bundle))))
    (testing "The magical girls and witches have positions."
      (is (every? #(not (nil? %))
                  (map :position
                       (concat (:magical-girls test-bundle)
                               (:witches bundle))))))))

(deftest test-summary-text
  (is (= (core/summary-text bundle)
         (str "Magical Girls: " starting-magical-girls "\n"
              "Witches: " starting-witches "\n"
              "Incubators: " incubator-count "\n"
              "Turns: " (:turns bundle)))))

;;;; Integration tests. 
