(ns aoc.d16
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse-field [s]
  (let [[_ n & nums] (re-matches #"([^:]+): (\d+)-(\d+) or (\d+)-(\d+)" s)
        [a b c d :as numbers] (map parse-int nums)]
    {:name n
     :numbers numbers
     :valid? (fn [k] (or (<= a k b) (<= c k d)))}))

(defn parse-ticket [s]
  (map parse-int (str/split s #",")))

(def input (let [s (str/split-lines (slurp (io/resource "d16.txt")))
                 [fields my nearby] (remove #(= [""] %) (partition-by str/blank? s))]
             {:fields (map parse-field fields)
              :my (parse-ticket (second my))
              :nearby (map parse-ticket (drop 1 nearby))}))

(defn field-checker
  [fields]
  (fn [n]
    (some  #((:valid? %) n) fields)))

(reduce + (remove (field-checker (:fields input)) (apply concat (:nearby input))))
