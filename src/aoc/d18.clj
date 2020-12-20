(ns aoc.d18
  (:require [clojure.string :as str]
            [instaparse.core :as insta]
            [clojure.java.io :as io]))

(def grammar "
  E=num|mul|add|par
  num=#'\\d+'
  mul=E <' * '> (num|par)
  add=E <' + '> (num|par)
  par=<'('> E <')'>
  ")

(defn parens
  "See: https://en.wikipedia.org/wiki/Operator-precedence_parser

  Trick to correctly have the operator precendence with LR parser:
  - replace + with ) + (
  - replace * with )) * ((
  - add (( at the beginning and after each left-paren in the original
  - add )) at the beginning and before each right-paren in the original
  "
  [s]
  (str "(("
       (-> s
           (str/replace #" " "")
           (str/replace #"\(" "(((")
           (str/replace #"\)" ")))")
           (str/replace #"\+" ") + (")
           (str/replace #"\*" ")) * (("))
       "))"))

(defn parse
  [s]
  (insta/transform
   {:E identity
    :num #(Integer/parseInt %)
    :add (fn [& args] (conj args '+))
    :mul (fn [& args] (conj args '*))
    :par identity}
   (insta/parse (insta/parser grammar) s)))

(def input (str/split-lines (slurp (io/resource "d18.txt"))))

(eval (parse "1 + 2 * 3 + 4 * 5 + 6"))
(eval (parse "1 + (2 * 3) + (4 * (5 + 6))"))
(eval (parse "2 * 3 + (4 * 5)"))
(eval (parse "5 + (8 * 3 + 9 + 3 * 4 * 3)"))
(eval (parse "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"))
(eval (parse "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"))
(->> input
     (map (comp eval parse))
     (reduce +))


(eval (parse (parens "1 + 2 * 3 + 4 * 5 + 6")))
(eval (parse (parens "1 + (2 * 3) + (4 * (5 + 6))")))
(eval (parse (parens "2 * 3 + (4 * 5)")))
(eval (parse (parens "5 + (8 * 3 + 9 + 3 * 4 * 3)")))
(eval (parse (parens "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")))
(eval (parse (parens "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")))
(->> input
     (map (comp eval parse parens))
     (reduce +))
