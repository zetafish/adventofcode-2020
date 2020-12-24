(ns aoc.d24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; https://www.redblobgames.com/grids/hexagons/

(defn parse-route
  [s]
  (:route
   (reduce (fn [acc ch]
             (case ch
               \n (assoc acc :north-south \n)
               \s (assoc acc :north-south \s)
               (let [seg  (str (:north-south acc) ch)]
                 (-> acc
                     (dissoc :north-south)
                     (update :route (fnil conj []) seg)))
               ))
           {}
           s)))

(def input (map parse-route
                (str/split-lines
                 (slurp (io/resource "d24.txt")))))

(def example (map parse-route
                  (str/split-lines
                   (slurp (io/resource "d24-example.txt")))))

(def d-even {"e"  [1 0]
             "w"  [-1 0]
             "se" [0 1]
             "sw" [-1 1]
             "ne" [0 -1]
             "nw" [-1 -1]})

(def d-odd {"e"  [1 0]
            "w"  [-1 0]
            "se" [1 1]
            "sw" [0 1]
            "ne" [1 -1]
            "nw" [0 -1]})

(defn step [[_ y :as point] d]
  (cond
    (even? y) (mapv + point (d-even d))
    (odd? y)  (mapv + point (d-odd d))))

(defn follow-directions [from route]
  (reduce step
          from
          route))

(defn black-tiles [routes]
  (->> routes
       (map #(reduce step [0 0] %))
       (frequencies)
       (filter (comp odd? second))
       count))

(black-tiles example)
(black-tiles input)
