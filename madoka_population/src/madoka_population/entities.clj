(ns madoka-population.entities
  (:require [incanter.stats :as stats]))

;; Magical girls' positions are assoced in later.
(defrecord MagicalGirl
    [combat tracking soul-gem corruption-rate home])

;; Witches' positions are built-in because they don't change.
(defrecord Witch
    [combat discoverability position])

(defrecord Familiar
    [combat])

(defrecord Incubator
    [success-rate])

(defprotocol combatant
  (can-flee [flier combat-diff])
  (blacken-soul-gem [entity multiplier]))

(extend-type MagicalGirl
  combatant
  (can-flee [magical-girl combat-diff]
    (< (Math/abs combat-diff) (:combat magical-girl)))
  (blacken-soul-gem [magical-girl multiplier]
    (assoc magical-girl
      :soul-gem
      (+ (:soul-gem magical-girl)
         (* multiplier (:corruption-rate magical-girl))))))

(extend-type Witch
  combatant
  (can-flee [_ _] false)
  (blacken-soul-gem [witch _]
    witch))

(extend-type Familiar
  combatant
  (can-flee [_ _] false)
  (blacken-soul-gem [familiar _]
    familiar))

(defn new-magical-girl
  "Creates a new magical girl with random stats."
  [world-size]
  {:post ()}
  (->MagicalGirl
   (stats/sample-exp 1 :rate 1/50)
   (stats/sample-exp 1 :rate 1/5)
   0.0
   (first (stats/sample-uniform 1 :min 0.01 :max 0.1))
   (vec (repeatedly 2 #(rand world-size)))))

(defn new-witch
  "Creates a new witch based on a magical girl."
  [magical-girl]
  (->Witch
   (* 1.5 (:combat magical-girl))
   (+ (:combat magical-girl)
      (* (:tracking magical-girl) (:tracking magical-girl)))
   (:position magical-girl)))

(defn new-incubator
  "Creates a new Incubator with random success rate."
  []
  (->Incubator
   (rand 1)))
