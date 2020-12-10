(ns aoc.d10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (mapv #(Long/parseLong %)
                 (str/split-lines (slurp (io/resource "d10.txt")))))

(def example1 [16 10 15 5 1 11 7 19 6 12 4])
(def example2 [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])

(defn jolt-distribution
  [coll]
  (let [coll (conj (sort coll) 0)]
    (frequencies (conj (map #(- %2 %1) coll (drop 1 coll)) 3))))

;; part 1
(jolt-distribution example1)
(jolt-distribution example2)
(reduce * (vals (jolt-distribution input)))
