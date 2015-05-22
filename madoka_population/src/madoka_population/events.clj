(ns madoka-population.events
  (:require [incanter.stats :as stats]
            [madoka-population.entities :as entities]))

(def ^:dynamic random-source
  "The source of random numbers. Can be rebound for reproducible
randomness."
  (java.util.Random.))

;;;; Utilities

(defn- rand-angle
  "Returns a random number between 0 and 2*pi, which can be
  interpreted as a radian angle / heading."
  []
  (* Math/PI (* 2 (.nextDouble random-source))))
;; Took the (* 2 (.nextDouble ...)) part from the source of rand.

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

(defn move
  "Moves a magical girl. Sends her home when appropriate."
  [{[x y :as position] :position, heading :heading} magical-girl
   within-world?
   return-home?]
  (assoc magical-girl :position
         (let [new-x (+ x (Math/cos heading))
               new-y (+ y (Math/sin heading))]
           (if (within-world? new-x new-y)
             [new-x new-y]
             (:home magical-girl)))))

(defn determine-heading
  "Pick a heading when necessary."
  [{position :position, home :home, heading :heading} magical-girl]
  (if (or (nil? heading) (= home position))
    (assoc magical-girl :heading (rand-angle))
    magical-girl))

;;;; Events which result in new entities.

(defn spawn-magical-girls
  "Each Incubator attempts to spawn a magical girl. Returns a sequence
  of successfully created magical girls in a diff map."
  [incubators world-size]
  (let [successes
        (->> (repeatedly (count incubators) #(.nextDouble random-source))
             (map #(when (<= %2 (:success-rate %1)) %2) incubators)
             (remove nil?))]
    {:magical-girls
     (repeatedly (count successes)
                 #(entities/new-magical-girl world-size))}))

(defn spawn-witches
  "Finds all magical girls who have succumbed to despair, and turns
  them into witches. Removes them from magical girl sequence and
  returns the new magical girl sequence and the new witch sequence in
  a diff map."
  [magical-girls witches]
  (let [despairing (into #{} (filter #(>= (:soul-gem %) 1.0)
                                     magical-girls))]
    {:magical-girls (remove despairing magical-girls)
     :witches (concat witches (map entities/new-witch despairing))}))

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
      {:winner stronger, :loser weaker}
      {:winner weaker, :loser stronger})))

(defn fight
  "Models combat between two entities. Returns a map of information
  about the outcome of the battle."
  [combatant1 combatant2]
  {:pre [(not-any? nil? (map :combat [combatant1 combatant2]))]}
  
  (let [info (get-combat-info combatant1 combatant2)
        fled? (attempt-escape (:weaker info) (:combat-diff info))]
    (cond
     fled?
       {:fled (:weaker info)}
     (zero? (:combat-diff info))
       (let [shuffled (shuffle [combatant1 combatant2])]
         {:winner (first shuffled)
          :loser (second shuffled)}) 
     :else
       (determine-outcome (:stronger info) (:weaker info)))))

(defn round-of-combat
  "Given all magical girls and witches on the field, runs all possible
  battles. Returns a list of surviving magical girls and witches."
  [magical-girls witches]
  (let [combat-results
        (->> (for [magical-girl magical-girls, witch witches
                   :when (discovered? magical-girl witch)]
               [magical-girl witch])
             (map #(apply fight %)))
        the-dead (into #{} (map :loser combat-results))
        the-victors (into #{} (map :winner combat-results))
        the-fled (into #{} (keep #(:fled % nil) combat-results))]
    {:magical-girls
     (->> magical-girls
         (remove the-dead)
         (map #(entities/blacken-soul-gem % 1))
         (map #(if (contains? the-victors %) (entities/won-battle %) %))
         (map #(if (contains? the-fled %) (entities/fled-battle %) %)))
     :witches
     (->> witches
          (remove the-dead))}))
;; Blacken everyone's soul gem, but then purify those who have won a battle.
;; After this we call spawn-witches on the list and anyone whose soul gem
;; is over the edge becomes a witch.
