(ns aoc.d5
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def example ["BFFFBBFRRR"])

(def input (str/split-lines (slurp (io/resource "d5.txt"))))

(defn jump [{:keys [n-cols n-rows] :as state} x]
  (case x
    \F (-> state (update :n-rows / 2))
    \B (-> state (update :n-rows / 2) (update :row + (/ n-rows 2)))
    \L (-> state (update :n-cols / 2))
    \R (-> state (update :n-cols / 2) (update :col + (/ n-cols 2)))))

(defn find-seat
  [s]
  (reduce jump {:row 0 :n-rows 128 :col 0 :n-cols 8} s))

(defn seat-id [{:keys [row col]}]
  (+ (* row 8) col))

(def seats (sort (map (comp seat-id find-seat) input)))

;; part 1
(last seats)

;; part 2
(->> (concat (partition 2 seats) (partition 2 (drop 1 seats)))
     (remove (fn [[a b]] (= 1 (- b a))))
     ffirst inc)
