(ns madoka-population.entities
  (:require [incanter.stats :as stats]))

(defrecord MagicalGirl
    [combat tracking soul-gem corruption-rate home])

(defrecord Witch
    [combat discoverability])

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

(defn- get-interval
  "Figures out the correct range of victory for the stronger
  combatant."
  [combat-diff]
  (if (pos? combat-diff)
    #(<= combat-diff % (* 2 combat-diff))
    #(<= (* 2 combat-diff) % combat-diff)))

(defn- logistic
  "The logistic function, y(z) = 1/(1 + e^(-z))"
  [z]
  (/ 1 (+ 1 (Math/exp (- z)))))


(defn get-combat-info
  "Calculates various quantities used for determining outcome of a
  battle."
  [combatant1 combatant2]
  (let [combatants (sort-by :combat
                            [combatant1 combatant2])]
    {:weaker (first combatants)
     :stronger (second combatants)
     :combat-diff (- (:combat combatant1) (:combat combatant2))}))

;; Note: For now we assume the weaker opponent is the one who
;; wants to escape, but nothing about this function requires that.
;; Also, nothing about this function cares whether the opponent
;; is a witch, a familiar, or another magical girl.
(defn attempt-escape
  "If one of the combatants is a magical girl and the difference in
  strength with her opponent is not too great, she may attempt to
  escape battle.

  Returns true for a successful escape, false otherwise."
  [flier combat-diff]
  (and (can-flee flier combat-diff)
       (< (rand) (/ (Math/abs combat-diff)
                    (* (:combat flier) (:combat flier))))))

(defn determine-outcome
  [stronger weaker]
  (if (> (rand) (/ (:combat weaker) (:combat stronger)))
    weaker
    stronger))

(defn fight
  "Models combat between two entities. Returns the winner."
  [combatant1 combatant2]
  {:pre [(not-any? nil? (map :combat [combatant1 combatant2]))]}
  
  (let [info (get-combat-info combatant1 combatant2)
        in-victory-interval (get-interval (:combat-diff info))
        fled? (attempt-escape (:weaker info) (:combat-diff info))]
    (cond
     fled?
     :fled 
     (zero? (:combat-diff info))
     (rand-nth [combatant1 combatant2])
     :else
     (determine-outcome (:stronger info) (:weaker info)))))
