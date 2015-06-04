(ns madoka-population.entities-test
  (:require [madoka-population.entities :as entities]
            [clojure.test :refer :all]))

(def kyouko
  (entities/map->MagicalGirl
   {:combat 93
    :tracking 13
    :soul-gem 0.0
    :corruption-rate 0.02
    :home [22 22]}))

(def walpurgisnacht
  (entities/map->Witch
   {:combat 1000
    :discoverability 100
    :position [345 346]}))

;; can-flee is tested in events_test, because I didn't feel like
;; re-creating all the infrastructure necessary to move that test
;; here.

;;;; Begin tests

(deftest test-blacken-soul-gem
  (testing "Kyouko's soul gem gets blackened."
    (is (= (:corruption-rate kyouko)
           (:soul-gem (entities/blacken-soul-gem kyouko 1)))))
  (testing "Kyouko's soul gem gets twice as black with multiplier 2."
    (is (= (* 2 (:corruption-rate kyouko))
           (:soul-gem (entities/blacken-soul-gem kyouko 2)))))
  (testing "Walpurgisnacht is unaffected."
    (is (= walpurgisnacht (entities/blacken-soul-gem walpurgisnacht 1)))))

(deftest test-increase-combat
  (testing "Kyouko's combat ability increases."
    (is (> (:combat (entities/increase-combat kyouko)) (:combat kyouko))))
  (testing "Walpurgisnacht's combat is unaffected."
    (is (= walpurgisnacht (entities/increase-combat walpurgisnacht)))))

(deftest test-won-battle
  (testing "When Kyouko wins, her combat is higher and her soul gem is clear."
    (let [victorious-kyouko (entities/won-battle
                             (entities/blacken-soul-gem kyouko 1))]
      (is (> (:combat victorious-kyouko) (:combat kyouko)))
      (is (zero? (:soul-gem victorious-kyouko)))))
  (testing "When Walpurgisnacht wins, she is unaffected."
    (is (= walpurgisnacht (entities/won-battle walpurgisnacht)))))

(deftest test-fled-battle
  (testing "When Kyouko runs away, her soul gem is twice as black."
    (let [cowardly-kyouko (entities/fled-battle kyouko)]
      (is (= (* 2 (:corruption-rate kyouko))
             (:soul-gem cowardly-kyouko)))))
  ;; Note: in events/round-of-combat, runaways get the non-battle
  ;; 1x corruption and then a 2x corruption on top of that, so they
  ;; get 3x corruption, but only 2x from fled-battle.
  (testing "Walpurgisnacht can't actually run, so is unaffected."
    (is (= walpurgisnacht (entities/fled-battle walpurgisnacht)))))

(deftest test-new-magical-girl
  (let [random-magical-girl (entities/new-magical-girl 200)]
    (testing "Random magical girl is positioned at home."
      (is (= (:home random-magical-girl) (:position random-magical-girl))))))
