(ns aoc.d2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def example ["1-3 a: abcde"
              "1-3 b: cdefg"
              "2-9 c: ccccccccc"])

(def input (str/split-lines (slurp (io/resource "d2.txt"))))

(defn parse
  [s]
  (let [p (re-find #"(\d+)-(\d+) (\w): (\w+)" s)]
    [(Integer/parseInt (p 1))
     (Integer/parseInt (p 2))
     (first (p 3))
     (vec (p 4))]))

(defn validate-1
  [[min max letter password]]
  (->> password
       (filter #(= letter %))
       count
       (#(<= min % max))))

(defn validate-2
  [[a b letter password]]
  (->> [(password (dec a)) (password (dec b))]
       (filter #(= letter %))
       count
       (#(= 1 %))))

;; part 1
(map (comp validate-1 parse) example)
(frequencies (map (comp validate-1 parse) input))

;; part 2
(map (comp validate-2 parse) example)
(frequencies (map (comp validate-2 parse) input))
