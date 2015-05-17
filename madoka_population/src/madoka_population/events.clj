(ns madoka-population.events
  (:require [incanter.stats :as stats]
            [madoka-population.entities :as entities]))

;; This is one ass-ugly function. Maybe a square macro would help.
(defn discovered?
  "Models whether a magical girl has discovered a witch. For now just
  checks if their discovery radii overlap, but later might have all
  that fancy stuff with the five rings."
  [magical-girl witch]
  (let [[mg-x mg-y] (:position magical-girl)
        [w-x w-y] (:position witch)
        magical-girl-radius (:tracking magical-girl)
        witch-radius (:discoverability witch)
        radius-difference (- magical-girl-radius witch-radius)
        radius-sum (+ magical-girl-radius witch-radius)
        x-difference (- mg-x w-x)
        y-difference (- mg-y w-y)]
    (<= (* radius-difference radius-difference)
        (+ (* x-difference x-difference) (* y-difference y-difference))
        (* radius-sum radius-sum))))
