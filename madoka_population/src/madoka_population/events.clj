(ns madoka-population.events
  (:require [incanter.stats :as stats]
            [madoka-population.entities :as entities]))

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

(defn spawn-magical-girls
  "Each Incubator attempts to spawn a magical girl."
  )
