(ns madoka-population.entities-test
  (:require [clojure.test :refer :all]
            [incanter.stats :as stats]
            [madoka-population.entities :as entities]))

(def sayaka
  (entities/->MagicalGirl 50 12 0 0.06 [34.123 109.234]))

(def gertrud
  (entities/->Witch 75 15 [-1 -1]))

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

(def sayaka-v-gertrud (entities/get-combat-info sayaka gertrud))

(def sayaka-v-gretchen
  (entities/get-combat-info sayaka kriemhild-gretchen))

(def ultimate-madoka-v-gretchen
  (entities/get-combat-info ultimate-madoka kriemhild-gretchen))

(deftest test-combat-sayaka-v-gertrud
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
    (is (false? (entities/can-flee sayaka (:combat-diff sayaka-v-gretchen)))))
  (testing "Kriemhild Gretchen cannot run from Sayaka."
    (is (false?
         (entities/can-flee kriemhild-gretchen
                   (:combat-diff sayaka-v-gretchen))))))


;;;; Below this is the printing tests. They print because they're random,
;;;; so there is no "right" answer to assert on, but there are more and less
;;;; acceptable answers, so I want it to just print the answers so I can
;;;; inspect them.

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
          (repeatedly ~rounds #(entities/fight ~combatant1 ~combatant2)))]
    [(str '~combatant1) (float (/ (get freqs# ~combatant1 0) ~rounds))
     (str '~combatant2) (float (/ (get freqs# ~combatant2 0) ~rounds))
     "fled" (float (/ (:fled freqs# 0) ~rounds))]))

(deftest test-fighting
  (testing "How often each opponent wins (Gertrud vs. Sayaka)."
    (apply (partial printf "%s: %.3f\n%s: %.3f\n%s: %.3f\n")
           (count-wins-of sayaka gertrud 1000)))
  (testing "How often each opponent wins (Sayaka vs. Gretchen)"
    (apply (partial printf "%s: %.3f\n%s: %.3f\n%s: %.3f\n")
           (count-wins-of sayaka kriemhild-gretchen 1000)))
  (testing "How often each opponent wins (Ultimate Madoka vs. Gretchen)"
    (apply (partial printf "%s: %.3f\n%s: %.3f\n%s: %.3f\n")
           (count-wins-of ultimate-madoka kriemhild-gretchen 1000)))
  (testing "How often each opponent wins (Sayaka vs. Anthony)"
    (apply (partial printf "%s: %.3f\n%s: %.3f\n%s: %.3f\n")
           (count-wins-of sayaka anthony 1000))))
