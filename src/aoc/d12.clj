(ns aoc.d12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse [s]
  [(keyword (subs s 0 1))
   (Integer/parseInt (subs s 1))])

(def input (map parse (str/split-lines (slurp (io/resource "d12.txt")))))

(def example (map parse ["F10" "N3" "F7" "R90" "F11"]))

(defn simple-nav [{:keys [degree] :as state} [action value]]
  (case action
    :N (update state :y + value)
    :S (update state :y - value)
    :E (update state :x + value)
    :W (update state :x - value)
    :L (update state :degree + value)
    :R (update state :degree - value)
    :F (-> state
           (update :x + (* value (Math/cos (Math/toRadians degree))))
           (update :y + (* value (Math/sin (Math/toRadians degree)))))))

(defn ->polar [[x y]]
  {:r (Math/sqrt (+ (* x x) (* y y)))
   :theta (Math/atan2 y x)})

(defn ->cartesian [{:keys [r theta]}]
  [(* r (Math/cos theta))
   (* r (Math/sin theta))])

(defn waypoint-nav [{:keys [waypoint ship] :as state} [action value]]
  (let [rotate (fn [op]
                 (->cartesian
                  (update (->polar waypoint) :theta op (Math/toRadians value))))]
    (case action
      :N (update-in state [:waypoint 1] + value)
      :S (update-in state [:waypoint 1] - value)
      :E (update-in state [:waypoint 0] + value)
      :W (update-in state [:waypoint 0] - value)
      :L (assoc state :waypoint (rotate +))
      :R (assoc state :waypoint (rotate -))
      :F (assoc state :ship (apply map + ship (repeat value waypoint))))))

(defn manhattan
  [coll]
  (->> coll
       (map #(Math/abs %))
       (reduce +)
       Math/round))

;; part 1
(manhattan ((juxt :x :y) (reduce simple-nav {:x 0 :y 0 :degree 0} example)))
(manhattan ((juxt :x :y) (reduce simple-nav {:x 0 :y 0 :degree 0} input)))

;; part 2
(manhattan (:ship (reduce waypoint-nav {:ship [0 0] :waypoint [10 1]} example)))
(manhattan (:ship (reduce waypoint-nav {:ship [0 0] :waypoint [10 1]} input)))
