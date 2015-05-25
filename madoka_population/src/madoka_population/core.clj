(ns madoka-population.core
  (:gen-class)
  (:require [madoka-population.entities :as entities]
            [madoka-population.events :as events]
            [madoka-population.state :as state]
            [quil.core :as qc]
            [quil.middleware :as qm]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

;;;;;;;;;;;;;; STUFF THAT WILL GO AWAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In the final program this will be added to the namespace by main
;; using the add-vars-to-ns macro.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def world-size 200)
(def incubator-count 10)
(def incubator-mean-success 0.5)
(def starting-magical-girls 0)
(def starting-witches 0)
;;;;;;;;;;;;;; END STUFF THAT GOES AWAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn new-state-bundle
  "Returns the initial state bundle for the simulation, based on the
  input parameters."
  []
  )

(defn update-state-bundle
  [old-bundle])

(defn draw
  [state-bundle])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; This will be gone in the final product. We'll use the main below
;;;; from a runnable jar. The defsketch and hardcoded input map are
;;;; for testing only.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(qc/defsketch test-sketch
  :title "Madoka Population Simulator"
  :setup new-state-bundle
  :update update-state-bundle
  :draw draw
  :middleware [qm/fun-mode]
  :features [:exit-on-close])
;;;;;;;;;;;;;;;;;;;; END STUFF THAT WILL GO AWAY ;;;;;;;;;;;;;;;;;;;;

(defn -main
  "Entry point for the program, initializes input parameters and
  starts the sketch going."
  [& args]
  (let [inputs (-> (first args)
                   slurp
                   edn/read-string)]
    (state/add-vars-to-ns inputs))
  (qc/sketch
   :title "Madoka Population Simulator"
   :setup new-state-bundle
   :update update-state-bundle
   :draw draw
   :middleware [qm/fun-mode]
   :features [:exit-on-close]))

;; 1. Put the input parameters (incubator number, mean success rate,
;; starting number of magical girls and witches [it should still be
;; able to work if both of those are zero]) in the namespace using
;; add-vars-to-ns.

;; 2. Inside the setup function, initialize the state bundle based on
;; those input parameters. If we want to let the Incubators try and
;; make contracts right away, use a let so the incubator list is
;; available as input to the spawn-magical-girls function.
