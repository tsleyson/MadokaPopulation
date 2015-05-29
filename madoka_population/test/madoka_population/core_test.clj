(ns madoka-population.core-test
  (:require [clojure.test :refer :all]
            [madoka-population.core :as core]))

;; Test parameter definitions
(def world-size 200)
(def incubator-count 10)
(def incubator-mean-success 0.5)
(def starting-magical-girls 5)
(def starting-witches 5)
(def bundle (core/new-state-bundle))

(deftest test-new-bundle
  (testing "The state gets initialized correctly."
    (is (= starting-magical-girls (count (:magical-girls bundle))))
    (is (= starting-witches (count (:witches bundle))))
    (is (= incubator-count (count (:incubators bundle))))
    (is ((:within-world? bundle) (/ world-size 2)))
    (is (zero? (:turns bundle))))
  (testing "The magical girls and witches have positions."
    (is (every? #(not (nil? %))
                (map :position
                     (concat (:magical-girls bundle) (:witches bundle)))))))

(deftest test-summary-text
  (is (= (core/summary-text bundle)
         (str "Magical Girls: " starting-magical-girls "\n"
              "Witches: " starting-witches "\n"
              "Incubators: " incubator-count "\n"
              "Turns: " (:turns bundle)))))

(deftest test-adding-config-state)
