(ns aoc.d7
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-one-inside
  [s]
  (let [[_ n name] (re-matches #"(\d+) (\w+ \w+) bags?" s)]
    [(Integer/parseInt n) name]))

(defn parse-inside
  [s]
  (map parse-one-inside (str/split s #", ")))

(defn parse-rule
  [s]
  (let [[_ name inside] (re-matches #"(\w+ \w+) bags contain (.*)\." s)]
    (if (= "no other bags" inside)
      [name nil]
      [name (parse-inside inside)])))

(defn build-graph
  [lines]
  (mapcat (fn [[outer inners]]
            (map (fn [[n inner]] [outer n inner]) inners))
          (map parse-rule lines)))

(def example
  ["light red bags contain 1 bright white bag, 2 muted yellow bags."
   "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
   "bright white bags contain 1 shiny gold bag."
   "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
   "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
   "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
   "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
   "faded blue bags contain no other bags."
   "dotted black bags contain no other bags."])

(def input (str/split-lines (slurp (io/resource "d7.txt"))))

(build-graph example)

(defn traverse-outers
  [edges from]
  (loop [outers #{} todo [from]]
    (if-not (seq todo)
      outers
      (let [found (->> edges
                       (filter #(= (first todo) (last %)))
                       (map first)
                       (remove outers))]
        (recur (into outers found)
               (into (rest todo) found))))))

;; part 1
(count (traverse-outers (build-graph example) "shiny gold"))
(count (traverse-outers (build-graph input) "shiny gold"))
