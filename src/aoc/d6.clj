(ns aoc.d6
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (str/split-lines (slurp (io/resource "d6.txt"))))

;; part 1
(->> input
     (partition-by str/blank?)
     (remove #{[""]})
     (map (comp count set (partial apply concat)))
     (reduce +))
