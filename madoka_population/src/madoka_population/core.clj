(ns madoka-population.core
  (:gen-class)
  (:require [madoka-population.entities :as entities]
            [quil.core :as qc]))

(def world-size
  "The size of the world map, in pixels"
  400)

;; Note: if errors occur inside an agent, you have to
;; call clear-agent-errors on it before proceeding.
(def dots
  (doall repeatedly (500
                     #(->> world-size
                           (repeat 2)
                           (mapv (partial * 0.5))
                           agent))))

(def dot-wait
  "When true, the dot should halt and wait."
  (atom true))

(def continue
  "When false, the main loop quits."
  (atom true))

(def frames-per-second 40)

(def update-lag
  "It's pointless to update data more than once during this period,
  because the screen won't be updated and we'll have weird leaps."
  (int (/ 1000 frames-per-second)))

;; Utility macro in case I decide (for whatever reason) to
;; change the representation of the flags; then I just change swap!
;; here.
(defmacro toggle
  "Toggle a flag."
  [flag]
  `(swap! ~flag not))

(defn update-dot
  "Moves dot one pixel in direction."
  [[x y :as pos] direction wait-time]
  (if @dot-wait
    pos
    (let [new-x (+ x (Math/cos direction))
          new-y (+ y (Math/sin direction))]
      (if (and (<= 0 new-x world-size) (<= 0 new-y world-size))
        (do
          (Thread/sleep wait-time)
          (send-off *agent* update-dot direction wait-time)
          [new-x new-y])
        (do
          (toggle dot-wait)
          (mapv (partial * 0.5) (repeat 2 world-size)))))))

(defn- background-thread
  "Repeatedly executes function on another thread."
  [function]
  (future
    (loop []
      (function)
      (when @continue
        (recur)))))

(defn- rand-angle
  "Returns a random number between 0 and 2*pi, which can be
  interpreted as a number of radians."
  []
  (* qc/PI (rand 2)))
;; Note: 0 radians = 2*pi radians, so we can exclude 2.

(defn move-dot
  "Sends update function to dot's agent."
  [wait-time]
  (when @dot-wait
    (toggle dot-wait))
  (doseq [dot dots]
    (send-off dot update-dot (rand-angle) (/ update-lag 2)))
  (Thread/sleep wait-time)
  (toggle dot-wait))

(defn setup
  []
  (qc/smooth)
  (qc/frame-rate frames-per-second) 
  (background-thread #(move-dot 20000)))

;; Coords to qc/text specify bottom left of bounding box.
(defn draw
  []
  (qc/background 150 150 150)
  (doseq [dot dots]
    (let [[x y] @dot]
      (qc/fill 250 150 150)
      (qc/ellipse x y 5 5)
      (qc/fill 0 0 0)
      #_(qc/text (format "x:%.3f\ny:%.3f\n" x y) 10 10))))

(qc/defsketch simple-example
  :title "A simple example"
  :setup setup
  :draw draw
  :size [world-size world-size])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, Madoka!"))
