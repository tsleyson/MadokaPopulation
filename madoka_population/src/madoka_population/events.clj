(ns madoka-population.events
  (:require [incanter.stats :as stats]
            [madoka-population.entities :as entities]))

(def ^:dynamic random-source
  "The source of random numbers. Can be rebound for reproducible
randomness."
  (java.util.Random.))

;;;; Movement-related functions

;; This is one ass-ugly function. Maybe a square macro would help.
;; Note: see http://stackoverflow.com/a/8367547/3376926.
;; We don't have a |R0 - R1| <= ... part because that excludes circles
;; which are contained in another circle, but we want to include those.
(defn circles-overlap?
  "Checks if discovery radii overlap."
  [magical-girl witch]
  {:pre (contains? magical-girl :position)}
  (let [[mg-x mg-y] (:position magical-girl)
        [w-x w-y] (:position witch)
        magical-girl-radius (:tracking magical-girl)
        witch-radius (:discoverability witch)
        radius-sum (+ magical-girl-radius witch-radius)
        x-difference (- mg-x w-x)
        y-difference (- mg-y w-y)]
    (<= (+ (* x-difference x-difference) (* y-difference y-difference))
        (* radius-sum radius-sum))))

(defn discovered?
  "Models whether a magical girl has discovered a witch. For now just
  checks if their discovery radii overlap, but later might have all
  that fancy stuff with the five rings."
  [magical-girl witch]
  (circles-overlap? magical-girl witch))

;;;; Events which result in new entities.

(defn spawn-magical-girls
  "Each Incubator attempts to spawn a magical girl."
  [incubators world-size]
  (let [successes
        (->> (repeatedly (count incubators) #(.nextDouble random-source))
             (map #(when (<= %2 (:success-rate %1)) %2) incubators)
             (remove nil?))]
    (repeatedly (count successes) #(entities/new-magical-girl world-size))))

;;;; Combat related functions

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
  (and (entities/can-flee flier combat-diff)
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
