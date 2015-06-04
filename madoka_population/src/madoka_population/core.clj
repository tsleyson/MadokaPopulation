(ns madoka-population.core
  (:gen-class)
  (:require [madoka-population.entities :as entities]
            [madoka-population.events :as events]
            [quil.core :as qc]
            [quil.middleware :as qm]
            [clojure.edn :as edn]))

(def default-config
  "The default config used when no map provided."
  '{world-size 200
    incubator-count 10
    incubator-mean-success 0.5
    starting-magical-girls 5
    starting-witches 5})

;;;; Helpers
(defn within-world-of-size?
  "Returns a function which tests whether its argument is between 0
  and size."
  [size]
  (fn [& points] (every? #(<= 0 % size) points)))

(defn summary-text
  "Returns a summary of the current state for printing on screen."
  [{:keys [magical-girls witches incubators turns] :as bundle}]
  (format (str "Magical Girls: %d\n"
               "Witches: %d\n"
               "Incubators: %d\n"
               "Turns: %d")
          (count magical-girls)
          (count witches)
          (count incubators)
          turns))

(defn get-config
  "Reads config map from a file."
  [filename]
  {:pre [(or (= java.lang.String (type filename)) (nil? filename))]}
  (if (nil? filename)
    default-config
    (-> filename
        slurp
        edn/read-string)))

;;;; State management

(defn setup-function
  "Returns a closure around the initial state bundle for the
  simulation, based on the input parameters."
  [{:syms
    [incubator-count incubator-mean-success starting-magical-girls
     starting-witches world-size turns-per-day]}
   & {:keys [testing] :or {testing false}}]
  (fn []
    (when (not testing)
      (qc/smooth)
      (qc/frame-rate 60))
    {:incubators (events/spawn-incubators
                  incubator-count incubator-mean-success)
     :magical-girls (repeatedly starting-magical-girls
                                #(entities/new-magical-girl world-size))
     :witches (map entities/new-witch
                   (repeatedly starting-witches
                               #(entities/new-magical-girl world-size)))
     ;; Make some magical girls and immediately turn them into witches
     ;; (the poor dears) to get the initial batch of witches.
     :turns 0
     :turns-per-day turns-per-day
     :within-world? (within-world-of-size? world-size)}))

(defn before-combat
  "Creates new magical girls and moves the magical girls for this
  turn. Returns a diff map of changes. Meant to be called in pipeline
  with events/round-of-combat and events/spawn-witches."
  [{:keys [magical-girls witches incubators turns turns-per-day] :as bundle}]
  {:magical-girls (->> magical-girls
                       (comp
                        (map events/move) 
                        (if (zero? (mod turns turns-per-day))
                          (partial map #(dissoc % :position))
                          identity)
                        (concat (events/spawn-magical-girls incubators))))
   :witches witches})

(defn update-state-bundle
  [bundle]
  (merge bundle
         (-> (before-combat bundle)
             events/round-of-combat
             events/spawn-witches)))

(defn draw
  [{:keys [magical-girls witches turns] :as bundle}]
  (qc/background 150 150 150)
  (qc/fill 0 0 0)
  (qc/text (summary-text bundle) 10 10)
  (qc/fill 250 150 150) ;; Mahou shoujo pink.
  (doseq [magical-girl magical-girls]
    (let [[x y] (:position magical-girl)]
      (qc/ellipse x y 5 5)))
  (qc/fill 0 0 0) ;; Witch black.
  (doseq [witch witches]
    (let [[x y] (:position witch)]
      (qc/ellipse x y 10 10))))

(defn -main
  "Entry point for the program, initializes input parameters and
  starts the sketch going."
  [& args]
  (qc/sketch
   :title "Madoka Population Simulator"
   :setup (-> (first args)
              get-config
              setup-function)
   :update update-state-bundle
   :draw draw
   :middleware [qm/fun-mode]
   ;:features [:exit-on-close]
   ))

;; 1. Put the input parameters (incubator number, mean success rate,
;; starting number of magical girls and witches [it should still be
;; able to work if both of those are zero]) in the namespace using
;; add-vars-to-ns.

;; 2. Inside the setup function, initialize the state bundle based on
;; those input parameters. If we want to let the Incubators try and
;; make contracts right away, use a let so the incubator list is
;; available as input to the spawn-magical-girls function.
