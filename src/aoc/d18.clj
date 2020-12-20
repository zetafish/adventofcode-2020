(ns aoc.d18
  (:require [clojure.string :as str]
            [instaparse.core :as insta]
            [clojure.java.io :as io]))

(def g "
  <E>=num|mul|add|par
  num=#'\\d+'
  mul=E <' * '> (num|par)
  add=E <' + '> (num|par)
  par=<'('> E <')'>
")

(defn parse [s]
  (first
   (insta/transform
    {:num #(Integer/parseInt %)}
    (insta/parse (insta/parser g) s))))

(def input (map parse (str/split-lines (slurp (io/resource "d18.txt")))))

(first input)

(defn eval-expr [expr]
  (cond
    (number? expr) expr
    (= :par (first expr)) (eval-expr (second expr))
    (= :mul (first expr)) (* (eval-expr (nth expr 1))
                             (eval-expr (nth expr 2)))
    (= :add (first expr)) (+ (eval-expr (nth expr 1))
                             (eval-expr (nth expr 2)))))

(eval-expr (parse "1 + 2 * 3 + 4 * 5 + 6"))
(eval-expr (parse "1 + (2 * 3) + (4 * (5 + 6))"))
(eval-expr (parse "2 * 3 + (4 * 5)"))
(eval-expr (parse "5 + (8 * 3 + 9 + 3 * 4 * 3)"))
(eval-expr (parse "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"))
(eval-expr (parse "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))
