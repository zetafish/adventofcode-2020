(ns aoc.d22
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-int [s] (Integer/parseInt s))

(def input (->> (str/split-lines (slurp (io/resource "d22.txt")))
                (partition-by str/blank?)
                (remove #{[""]})
                (map #(drop 1 %))
                (map #(map parse-int %))
                ))

(def example [[9 2 6 3 1] [5 8 4 7 10]])

(defn combat
  [d1 d2]
  (cond
    (empty? d1) [:player-2 d2]
    (empty? d2) [:player-1 d1]
    :else (let [c1 (first d1)
                c2 (first d2)]
            (cond
              (< c1 c2) (recur (rest d1) (concat (rest d2) [c2 c1]))
              (< c2 c1) (recur (concat (rest d1) [c1 c2]) (rest d2))))))

(defn score [deck]
  (reduce + (map-indexed (fn [i c] (* c (inc i))) (reverse deck))))

;; part 1
(score (second (combat (first example) (second example))))
(score (second (combat (first input) (second input))))

(defn recursive-combat
  ([d1 d2] (recursive-combat #{} d1 d2))
  ([seen d1 d2]
   (cond
     (contains? seen [d1 d2]) [:player-1 d1 d2]
     (empty? d1) [:player-2 d2]
     (empty? d2) [:player-1 d1]
     :else (let [c1 (first d1)
                 c2 (first d2)
                 winner (cond
                          (and (< c1 (count d1)) (< c2 (count d2)))
                          (first (recursive-combat #{}
                                                   (take c1 (drop 1 d1))
                                                   (take c2 (drop 1 d2))))
                          (< c1 c2) :player-2
                          (< c2 c1) :player-1)]
             (case winner
               :player-2 (recur (conj seen [d1 d2])
                                (rest d1)
                                (concat (rest d2) [c2 c1]))
               :player-1 (recur (conj seen [d1 d2])
                                (concat (rest d1) [c1 c2])
                                (rest d2)))))))

;; part 2
(recursive-combat [9, 2, 6, 3, 1] [5, 8, 4, 7, 10])
(score (second (recursive-combat (first example) (second example))))
(score (second (recursive-combat (first input) (second input))))
