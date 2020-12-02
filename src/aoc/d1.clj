(ns aoc.d1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (map #(Integer/parseInt %)
                (str/split-lines (slurp (io/resource "d1.txt")))))

;; part 1
(for [x input y input :when (and (< x y) (= 2020 (+ x y)))]
  (* x y))

;; part 2
(for [x input y input z input :when (and (< x y z) (= 2020 (+ x y z)))]
  (* x y z))
