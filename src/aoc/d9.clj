(ns aoc.d9
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.zip :as z]))

(def input (mapv #(Long/parseLong %)
                 (str/split-lines (slurp (io/resource "d9.txt")))))

(def example [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])

(defn preamble-match?
  [preamble n]
  (seq
   (let [preamble (into [] (zipmap (iterate inc 0) preamble))]
     (for [x preamble
           y preamble
           :when (and (not= x y) (= n (+ (second x) (second y))))]
       [x y]))))

(defn find-bad-number
  [coll size]
  (loop [zp (->> (z/vector-zip coll) (iterate z/next) (drop (inc size)) first)
         pos size]
    (let [x (z/node zp)
          k (- pos size)]
      (if-not (preamble-match? (drop k (z/lefts zp)) x)
        x
        (recur (z/next zp) (inc pos))))))

;; part 1
(find-bad-number example 5)
(find-bad-number input 25)
