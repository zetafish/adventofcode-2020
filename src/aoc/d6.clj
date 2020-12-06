(ns aoc.d6
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def input
  (->>
   (str/split-lines (slurp (io/resource "d6.txt")))
   (partition-by str/blank?)
   (remove #{[""]})))

;; part 1
(->> input
     (map (comp count set (partial apply concat)))
     (reduce +))

;; part 2
(->> input
     (map (comp count (partial apply set/intersection) (partial map set)))
     (reduce +))
