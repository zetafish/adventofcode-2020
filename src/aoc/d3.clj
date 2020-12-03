(ns aoc.d3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (mapv vec (str/split-lines (slurp (io/resource "d3.txt")))))

(def example
  (mapv vec ["..##......."
             "#...#...#.."
             ".#....#..#."
             "..#.#...#.#"
             ".#...##..#."
             "..#.##....."
             ".#.#.#....#"
             ".#........#"
             "#.##...#..."
             "#...##....#"
             ".#..#...#.#"]))

(defn count-trees
  [grid slope]
  (let [n-row (count grid)
        n-col (count (first grid))]
    (->> (iterate (partial map + slope) [0 0])
         (take-while #(< (second %) n-row))
         (map (fn [[x y]] [y (rem x n-col)]))
         (map #(get-in grid %))
         (filter #{\#})
         count)))

;; part 1
(count-trees example [3 1])
(count-trees input [3 1])

;; part 2
(def slopes [[1 1] [3 1] [5 1] [7 1] [1 2]])
(apply * (map (partial count-trees example) slopes))
(println (apply * (map (partial count-trees input) slopes)))
