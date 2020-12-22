(ns aoc.d22
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-int [s] (Integer/parseInt s))

(def decks (->> (str/split-lines (slurp (io/resource "d22.txt")))
                (partition-by str/blank?)
                (remove #{[""]})
                (map #(drop 1 %))
                (map #(map parse-int %))
                ))

(def example [[9 2 6 3 1] [5 8 4 7 10]])

(defn play [d1 d2]
  (cond
    (empty? d1) [:player-2 d2]
    (empty? d2) [:player-1 d1]
    :else (let [c1 (first d1)
                c2 (first d2)]
            (cond
              (< c1 c2) (recur (rest d1) (concat (rest d2) [c2 c1]))
              (< c2 c1) (recur (concat (rest d1) [c1 c2]) (rest d2))))))

(play (first example) (second example))
