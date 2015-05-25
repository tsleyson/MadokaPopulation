(ns madoka-population.events-test
  (:require [clojure.test :refer :all]
            [incanter.stats :as stats]
            [madoka-population.events :as events]
            [madoka-population.entities :as entities]))

;;;; IDENTIFICATION SECTION

(def world-size 200)

(def sayaka
  (entities/->MagicalGirl 50 12 0 0.06 [34.123 109.234]))

(def gertrud
  (entities/->Witch 75 15 [50 50]))

(def mami
  (entities/->MagicalGirl 100 15.0 0 0 [0 0]))

(def charlotte
  (entities/->Witch 150 24.0 [10 10]))

;; One of Gisela's familiars.
(def anthony
  (entities/->Familiar 49))

;; Kriemhild Gretchen is Madoka's witch form, capable of
;; destroying the world in ten days and pretty hard not to
;; notice, so it has 1 billion combat and discoverability.
(def kriemhild-gretchen
  (entities/->Witch 1e9 1e9 [-1 -1]))

;; Ultimate Madoka doesn't succumb to despair and she
;; can defeat any witch, even her own witch form which
;; can destroy the entire universe, and she can find any
;; witch anywhere, so her tracking and combat are a trillion
;; and her soul gem degradation is 0.
(def ultimate-madoka
  (entities/->MagicalGirl 1e12 1e12 0 0 [0 0]))

(def incubators
  (map entities/->Incubator [0.5 0.9 0.7 0.43]))

(def sayaka-v-gertrud (events/get-combat-info sayaka gertrud))

(def sayaka-v-gretchen
  (events/get-combat-info sayaka kriemhild-gretchen))

(def ultimate-madoka-v-gretchen
  (events/get-combat-info ultimate-madoka kriemhild-gretchen))

;; This is a hack to test private methods.
(def randoms-in-range #'madoka-population.events/randoms-in-range)

;;;; Helper functions and macros.
(defn within-world?
  [& points]
  (every? #(<= 0 % world-size) points))

;; Note: I was moved to tears because the second I realized I could
;; get the original symbol as a string by using a macro, so I could
;; write (count-wins-of-1000 sayaka gertrud) and actually get back
;; the strings "sayaka" and "gertrud" without adding extra parameters
;; to the records, I went and wrote the macro, and it worked on the first
;; try even though I'm terrible at macros.
(defmacro count-wins-of
  [combatant1 combatant2 rounds]
  `(let [freqs#
         (frequencies
          (repeatedly ~rounds
                      #(let [ret# (events/fight ~combatant1 ~combatant2)]
                         (if (contains? ret# :fled)
                           :fled
                           (:winner ret#)))))]
    [(str '~combatant1) (float (/ (get freqs# ~combatant1 0) ~rounds))
     (str '~combatant2) (float (/ (get freqs# ~combatant2 0) ~rounds))
     "fled" (float (/ (:fled freqs# 0) ~rounds))]))

;;;; Henceforth begin the tests.

(deftest test-randoms-in-range
  (testing "Normal use."
    (let [sample (randoms-in-range 10 :mean 0.5 :sd 0.1)]
      (is (= 10 (count sample)))
      (is (every? #(<= 0 % 1) sample))))
  (testing "Without passing a standard deviation"
    (let [sample (randoms-in-range 10 :mean 0.5)]
      (is (= 10 (count sample)))
      (is (every? #(<= 0 % 1) sample)))))

(deftest test-circle-overlap
  (testing "Overlapping circles are detected."
    (is (events/circles-overlap? (assoc mami :position [35 35]) charlotte)))
  (testing "A circle inside a circle counts as overlapping."
    (is (events/circles-overlap? (assoc mami :position [5 5]) charlotte)))
  (testing "Non-overlapping circles aren't detected."
    (is (not (events/circles-overlap?
              (assoc mami :position [300 300]) charlotte))))
  (testing "The precondition fails if the magical girl has no position."
    (is (thrown? AssertionError (events/circles-overlap? mami charlotte)))))

;;; Note: Incanter can plot circles. The following code was hugely helpful
;;; for debugging and developing these test cases:
(comment
  (defn circle [h k r]
    (fn [t] [(+ h (* r (incanter.core/cos t)))
             (+ k (* r (incanter.core/sin t)))]))
  (-> (parametric-plot (circle 35 35 15) (- Math/PI) Math/PI)
      (add-parametric (circle 10 10 24) (- Math/PI) Math/PI)
      view))
;;; That code plots two circles with centers at 35, 35 and 10, 10 and radii
;;; 15 and 24. 

(deftest test-heading-determination
  (let [headed-mami (-> mami
                        (assoc :heading Math/PI)
                        (assoc :position [345 50]))]
    (testing "When she has a heading and isn't at home, nothing happens."
      (is (= Math/PI (:heading (events/determine-heading headed-mami)))))
    (testing "When she's at home, she gets a new heading."
      (binding [events/random-source (java.util.Random. 22)]
        (is (= 4.6006859922885575
               (:heading (events/determine-heading
                          (assoc headed-mami
                            :position (:home headed-mami))))))))
    (testing "When she has no heading, she gets one."
      (binding [events/random-source (java.util.Random. 22)]
        (is (= 4.6006859922885575
               (:heading (events/determine-heading
                          (dissoc headed-mami :heading)))))))))
;; N.B. Headed Mami still has her head.

(deftest test-movement
  (let [positioned-mami (-> mami
                            (assoc :position [5 5])
                            (assoc :heading Math/PI))]
    (testing "Regular movement works."
      (is (= [4.0 5.0]
             (:position (events/move positioned-mami within-world?)))))
    (testing "Movement with no position sets her at home."
      (is (= (:home mami)
             (:position (events/move (assoc mami :heading Math/PI)
                                     within-world?)))))
    (testing "Moving beyond the world sets her at home."
      (is (= (:home positioned-mami)
             (:position
              (events/move positioned-mami #(every? zero? %&))))))))

(deftest test-mg-spawning
  (let [new-girls (:magical-girls
                   (binding [events/random-source (java.util.Random. 22)]
                     (events/spawn-magical-girls incubators world-size)))]
    (testing "There are two new girls."
      (is (= 2 (count new-girls))))
    (testing "The new girls' homes are in the world."
      (is (->> new-girls
               :home
               (every? (fn [[x y]]
                         (and (<= 0 x world-size)
                              (<= 0 y world-size)))))))))

(deftest test-witch-spawning
  (let [{:keys [magical-girls witches]}
        (events/spawn-witches [(assoc sayaka :soul-gem 1.0)
                               (assoc mami :soul-gem 1.0)
                               ultimate-madoka]
                              [gertrud
                               kriemhild-gretchen])]
    (testing "There are two new witches."
      (is (= 4 (count witches))))
    (testing "Only Ultimate Madoka remains as a magical girl."
      (is (= 1 (count magical-girls))) 
      (is (= ultimate-madoka (first magical-girls))))))

(deftest test-combat-info
  (testing "The sayaka-v-gertrud map has the right keys."
    (is (= #{:weaker :stronger :combat-diff}
           (into #{} (keys sayaka-v-gertrud)))))
  (testing "Sayaka is weaker"
    (is (= sayaka (:weaker sayaka-v-gertrud))))
  (testing "Gertrud is stronger"
    (is (= gertrud (:stronger sayaka-v-gertrud))))
  (testing "The combat differential is correct."
    (is (= -25 (:combat-diff sayaka-v-gertrud))))
  (testing "Ultimate Madoka is stronger"
    (is (= ultimate-madoka (:stronger ultimate-madoka-v-gretchen)))))

(deftest test-running-away
  (testing "Sayaka can run from Gertrud"
    (is (true? (entities/can-flee sayaka (:combat-diff sayaka-v-gertrud)))))
  (testing "Sayaka cannot run from Kriemhild Gretchen."
    (is (false? (entities/can-flee sayaka
                                   (:combat-diff sayaka-v-gretchen)))))
  (testing "Kriemhild Gretchen cannot run from Sayaka due to being a witch."
    (is (false?
         (entities/can-flee kriemhild-gretchen
                   (:combat-diff sayaka-v-gretchen))))))

;; Note: last I checked, determine-outcome said Sayaka always won,
;; so I don't know how reliable or meaningful this test is. But
;; in the end, if you call fight, it seems to work. For now, a mystery
;; which I have neither time nor energy to solve.
(deftest test-rebound-randoms
  (testing "When we rebind random-source, we get the same answer."
    (let [first-attempt (binding
                            [events/random-source (java.util.Random. 22)]
                          (doall (repeatedly 100 #(events/determine-outcome
                                                   sayaka
                                                   gertrud))))
          second-attempt (binding
                             [events/random-source (java.util.Random. 22)]
                           (doall (repeatedly 100 #(events/determine-outcome
                                                    sayaka
                                                    gertrud))))]
      (is (= first-attempt second-attempt)))))

(deftest test-fighting-deterministically
  (testing "With a seed of 22, we get the expected results."
    (let [results (binding [events/random-source (java.util.Random. 22)]
                    (doall (repeatedly 5 #(events/fight sayaka gertrud))))]
      (is (= [sayaka gertrud sayaka gertrud sayaka]
             (map :winner results))))))

;;;; Below this is the printing tests. They print because they're random,
;;;; so there is no "right" answer to assert on, but there are more and less
;;;; acceptable answers, so I want it to just print the answers so I can
;;;; inspect them.

(deftest test-fighting
  (testing "How often each opponent wins (Gertrud vs. Sayaka)."
    (apply (partial printf "%s: %.3f\n%s: %.3f\n%s: %.3f\n")
           (count-wins-of sayaka gertrud 1000)))
  (testing "How often each opponent wins (Sayaka vs. Gretchen)"
    (apply (partial printf "%s: %.3f\n%s: %.3f\n%s: %.3f\n")
           (count-wins-of sayaka kriemhild-gretchen 1000)))
  (testing "How often each opponent wins (Mami vs. Gertrud)"
    (apply (partial printf "%s: %.3f\n%s: %.3f\n%s: %.3f\n")
           (count-wins-of mami gertrud 1000)))
  (testing "How often each opponent wins (Mami vs. Charlotte)"
    (apply (partial printf "%s: %.3f\n%s: %.3f\n%s: %.3f\n")
           (count-wins-of mami charlotte 1000)))
  (testing "How often each opponent wins (Ultimate Madoka vs. Gretchen)"
    (apply (partial printf "%s: %.3f\n%s: %.3f\n%s: %.3f\n")
           (count-wins-of ultimate-madoka kriemhild-gretchen 1000)))
  (testing "How often each opponent wins (Sayaka vs. Anthony)"
    (apply (partial printf "%s: %.3f\n%s: %.3f\n%s: %.3f\n")
           (count-wins-of sayaka anthony 1000))))
