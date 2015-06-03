(ns madoka-population.events
  (:require [incanter.stats :as stats]
            [madoka-population.entities :as entities]
            [clojure.set :as set]))

(def ^:dynamic random-source
  "The source of random numbers. Can be rebound for reproducible
randomness."
  (java.util.Random.))

;;;; Utilities
(defn- in-0->1
  [x]
  (<= 0 x 1))

(defn- rand-angle
  "Returns a random number between 0 and 2*pi, which can be
  interpreted as a radian angle / heading."
  []
  (* Math/PI (* 2 (.nextDouble random-source))))
;; Took the (* 2 (.nextDouble ...)) part from the source of rand.

(defn pick-your-battles
  "Implements a policy for magical girls to choose which battle to
  engage in, among all the possible battles in a turn."
  [[magical-girl opponents]]
  [magical-girl
   (->> opponents
        (sort-by :combat)
        first)])  ; Choose to fight weakest.

(defn- randoms-in-range
  "Generates a sample from a random normal distribution bound to the
  range between 0 and 1."
  [size & {:keys [mean sd] :or {mean 0.5, sd 0.01}}]
  (loop [sample (stats/sample-normal size :mean mean :sd sd)]
    (if (every? in-0->1 sample)
      sample
      (let [valid-sample (filter in-0->1 sample)]
        (recur
         (concat valid-sample
                 (stats/sample-normal
                  (- size (count valid-sample)) :mean mean :sd sd)))))))

;;;; Movement-related functions

;; Note: see http://stackoverflow.com/a/8367547/3376926.
;; We don't have a |R0 - R1| <= ... part because that excludes circles
;; which are contained in another circle, but we want to include those.
(defn circles-overlap?
  "Checks if discovery radii overlap."
  [magical-girl witch]
  {:pre [(contains? magical-girl :position)]}
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

(defn determine-heading
  "Pick a heading when necessary."
  [{:keys [position home heading] :as magical-girl}]
  {:pre [(contains? magical-girl :position)]}
  (if (or (nil? heading) (= home position))
    (assoc magical-girl :heading (rand-angle))
    magical-girl))

(defn move
  "Moves a magical girl. Sends her home when appropriate."
  [{[x y :as position] :position, :keys [heading home] :as magical-girl}
   within-world?]
  (if (or (nil? position) (nil? heading))
    (-> magical-girl
        (assoc :position home)
        (determine-heading))
    (assoc magical-girl :position
           (let [new-x (+ x (Math/cos heading))
                 new-y (+ y (Math/sin heading))]
             (if (within-world? new-x new-y) 
               [new-x new-y]
               home)))))

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

(defn spawn-incubators
  "Called once at the beginning of the simulation."
  [incubator-count mean-incubator-success]
  (map entities/->Incubator
       (randoms-in-range incubator-count :mean mean-incubator-success)))

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
       (< (.nextDouble random-source) (/ (Math/abs combat-diff)
                    (* 2 (:combat flier))))))

(defn get-victory-interval
  "Returns interval for a victory for the stronger party."
  [combat-ratio]
  (if (> combat-ratio (- 1 combat-ratio))
    ;; Then [0, combat-ratio] is a larger interval.
    #(<= 0 % combat-ratio)
    #(<= 0 % (- 1 combat-ratio))))

(defn determine-outcome
  "A helper for fight that determines who wins the confrontation."
  [stronger weaker]
  (let [ratio (/ (:combat weaker) (:combat stronger))
        in-victory-interval? (get-victory-interval ratio)]
    (if (in-victory-interval? (.nextDouble random-source))
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

(defn possible-battles
  "Returns a map of magical girls to vectors of witches in range."
  [magical-girls witches]
  (->> (for [magical-girl magical-girls, witch witches
             :when (discovered? magical-girl witch)]
         {magical-girl [witch]})
       (apply merge-with (comp vec concat))))

(defn combat-results
  "Run all possible battles given a list of witches and a list of
  magical girls. Returns a map containing the dead, the victors, and
  the fled."
  [magical-girls witches]
  {:post [(= #{} (set/intersection (:the-dead %)
                                   (:the-victors %)
                                   (:the-fled %)))
          (= (count (:the-dead %)) (count (:the-victors %)))]}
  (let [results (->> (possible-battles magical-girls witches)
                     (map pick-your-battles)
                     (map (comp vec reverse))
                     (into {})
                     (map #(fight (second %) (first %))))]
    {:the-dead (into #{} (map :loser results))
     :the-victors (into #{} (map :winner results))
     :the-fled (into #{} (keep #(:fled % nil) results))}))

(defn round-of-combat
  "Given all magical girls and witches on the field, runs all possible
  battles. Returns a list of surviving magical girls and witches."
  [magical-girls witches]
  (let [{:keys [the-dead the-victors the-fled]}
        (combat-results magical-girls witches)]
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
