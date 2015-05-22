(ns madoka-population.core
  (:gen-class)
  (:require [madoka-population.entities :as entities]
            [quil.core :as qc]
            [quil.middleware :as qm]
            [taoensso.timbre.profiling :as profiling]))

;; Note: Math/PI actually has more digits than quil.core/PI.
(defn rand-angle
  "Returns a random number between 0 and 2*pi, which can be
  interpreted as a number of radians."
  []
  (* qc/PI (rand 2)))

(def world-size
  "The size of the world map, in pixels"
  600)

(def turns-per-day
  "A multiple of 60, the frames per second."
  60)

(def frames-per-second 60)

(def initial-pinks 1000)
(def initial-blacks 200)

#_(profiling/defnp update-pinks
  "Moves dot one pixel in direction."
  [{[x y :as position] :position, heading :heading :as pink}, turns]
  (let [new-x (+ x (Math/cos heading))
        new-y (+ y (Math/sin heading))]
    (if (and (<= 0 new-x world-size) (<= 0 new-y world-size))
      {:position [new-x, new-y]
       :heading (if (zero? turns) (rand-angle) heading)}
      (do
        (toggle dot-wait)
        (mapv (partial * 0.5) (repeat 2 world-size))))))

(defn update-pink
  "Returns updated pink dot."
  [{[x y :as position] :position, heading :heading} turns]
  {:position
   (let [new-x (+ x (Math/cos heading))
         new-y (+ y (Math/sin heading))]
     (if (and (<= 0 new-x world-size) (<= 0 new-y world-size))
       [new-x new-y]
       (mapv (partial * 0.5) (repeat 2 world-size))))
   :heading
   (if (zero? turns)
     (rand-angle)
     heading)})

(comment (defn- background-thread
           "Repeatedly executes function on another thread."
           [function]
           (future
             (loop []
               (function)
               (when @continue
                 (recur))))))

(defn create-pink
  "This will create magical girls in the final version."
  []
  {:position (->> world-size
                  (repeat 2)
                  (mapv (partial * 0.5)))
   :heading (rand-angle)})

(defn create-black
  "Will create witches in the final version."
  []
  {:position (->> #(rand world-size)
                  (repeatedly 2)
                  vec)})

(defn setup
  []
  (qc/smooth)
  (qc/frame-rate frames-per-second) 
  {:turns 0
   :pinks (doall (repeatedly initial-pinks create-pink))
   :blacks (doall (repeatedly initial-blacks create-black))})

(defn update
  [{:keys [pinks blacks turns] :as previous-state}]
  #_(let [survivors
        (into #{}
              (for [pink pinks, black blacks
                    :when (discovery-zones-intersect? pink black)]
                ))])
  (-> previous-state
      (merge
       {:pinks
        (doall (pmap update-pink pinks (repeat turns)))
        :turns
        (mod (inc turns) turns-per-day)})))

;; Coords to qc/text specify bottom left of bounding box.
(defn draw
  [{:keys [pinks blacks turns]}]
  (qc/background 150 150 150)
  (qc/text (format "Turn %d" turns) 10 10)
  (doseq [pink pinks]
    (let [[x y] (:position pink)]
      (qc/fill 250 150 150)
      (qc/ellipse x y 5 5)
      #_(qc/text (format "x:%.3f\ny:%.3f\n" x y) 10 10)))
  (doseq [black blacks]
    (let [[x y] (:position black)]
      (qc/fill 0 0 0)
      (qc/ellipse x y 10 10))))

(qc/defsketch simple-example
  :title "A simple example"
  :setup setup
  :draw draw
  :update update
  :size [world-size world-size]
  :middleware [qm/fun-mode])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, Madoka!"))
