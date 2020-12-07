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
  (let [edges (mapcat (fn [[outer inners]]
                         (map (fn [[n inner]] [outer n inner]) inners))
                       (map parse-rule lines))]
    {:nodes (set (mapcat (juxt first last) edges))
     :edges edges}))

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

(def x2
  ["shiny gold bags contain 2 dark red bags."
   "dark red bags contain 2 dark orange bags."
   "dark orange bags contain 2 dark yellow bags."
   "dark yellow bags contain 2 dark green bags."
   "dark green bags contain 2 dark blue bags."
   "dark blue bags contain 2 dark violet bags."
   "dark violet bags contain no other bags."])

(def input (str/split-lines (slurp (io/resource "d7.txt"))))

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
(count (traverse-outers (:edges (build-graph example)) "shiny gold"))
(count (traverse-outers (:edges (build-graph input)) "shiny gold"))

(defn count-inside
  [edges from]
  (loop [insides [] todo [[1 from]]]
    (if-not (seq todo)
      insides
      (let [[n name] (first todo)
            found (->> edges
                       (filter #(= name (first %)))
                       (map (fn [[_ n' name']]
                              [(* n n') name'])))]
        (recur (into insides found)
               (into (rest todo) found))))))

;; part 2
(reduce + (map first (count-inside (:edges (build-graph example)) "shiny gold")))
(reduce + (map first (count-inside (:edges (build-graph x2)) "shiny gold")))
(reduce + (map first (count-inside (:edges (build-graph input)) "shiny gold")))
