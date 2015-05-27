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
  (blacken-soul-gem [entity multiplier])
  (increase-combat [entity])
  (won-battle [entity])
  (fled-battle [entity]))

(extend-type MagicalGirl
  combatant
  (can-flee [magical-girl combat-diff]
    (< (Math/abs combat-diff) (:combat magical-girl)))
  (blacken-soul-gem [magical-girl multiplier]
    (assoc magical-girl
      :soul-gem
      (+ (:soul-gem magical-girl)
         (* multiplier (:corruption-rate magical-girl)))))
  (increase-combat [magical-girl]
    (assoc magical-girl
      :combat
      (+ (:combat magical-girl)
         (first (stats/sample-normal 1 :mean 5 :sd 1)))))
  (won-battle [magical-girl]
    (-> magical-girl
        increase-combat
        (assoc :soul-gem 0.0)))
  (fled-battle [magical-girl]
    (blacken-soul-gem magical-girl 2)))

(extend-type Witch
  combatant
  (can-flee [_ _] false)
  (blacken-soul-gem [witch _]
    witch)
  (increase-combat [witch]
    witch)
  (won-battle [witch]
    witch)
  (fled-battle [witch]
    witch))

(extend-type Familiar
  combatant
  (can-flee [_ _] false)
  (blacken-soul-gem [familiar _]
    familiar)
  (increase-combat [familiar]
    familiar)
  (won-battle [familiar]
    familiar)
  (fled-battle [familiar]
    familiar))

(defn new-magical-girl
  "Creates a new magical girl with random stats."
  [world-size]
  (let [home (vec (repeatedly 2 #(rand world-size)))]
    (assoc (->MagicalGirl
            (stats/sample-exp 1 :rate 1/50)
            (stats/sample-exp 1 :rate 1/5)
            0.0
            (first (stats/sample-uniform 1 :min 0.01 :max 0.1))
            home)
      :position home)))

(defn new-witch
  "Creates a new witch based on a magical girl."
  [magical-girl]
  (->Witch
   (* 1.5 (:combat magical-girl))
   (+ (:combat magical-girl)
      (* (:tracking magical-girl) (:tracking magical-girl)))
   (:position magical-girl)))

;; Not really used, in favor of events/spawn-incubators, which lets
;; us control the mean success rate.
(defn new-incubator
  "Creates a new Incubator with random success rate."
  []
  (->Incubator
   (rand 1)))
