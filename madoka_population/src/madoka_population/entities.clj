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
                    (* 2 (:combat flier))))))

(defn get-victory-interval
  "Returns interval for a victory for the stronger party."
  [combat-ratio]
  (if (> combat-ratio (- 1 combat-ratio))
    ;; Then [0, combat-ratio] is a larger interval.
    #(<= 0 % combat-ratio)
    #(<= 0 % (- 1 combat-ratio))))

(defn determine-outcome
  [stronger weaker]
  (let [ratio (/ (:combat weaker) (:combat stronger))
        in-victory-interval? (get-victory-interval ratio)]
    (if (in-victory-interval? (rand))
      stronger
      weaker)))

(defn fight
  "Models combat between two entities. Returns the winner."
  [combatant1 combatant2]
  {:pre [(not-any? nil? (map :combat [combatant1 combatant2]))]}
  
  (let [info (get-combat-info combatant1 combatant2)
        fled? (attempt-escape (:weaker info) (:combat-diff info))]
    (cond
     fled?
     :fled 
     (zero? (:combat-diff info))
     (rand-nth [combatant1 combatant2])
     :else
     (determine-outcome (:stronger info) (:weaker info)))))
