(ns aoc.d17
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (str/split-lines (slurp (io/resource "d17.txt"))))

(defn build-sky
  [plane]
  (let [plane (mapv vec plane)
        max-x (count (first plane))
        max-y (count plane)]
    (set (for [x (range max-x)
               y (range max-y)
               :when (= \# (get-in plane [y x]))]
           [x y 0 0]))))

(def sky (build-sky input))

(def example (build-sky [".#." "..#" "###"]))

(defn neighbors [p]
  (map #(map + p %)
       (for [x [-1 0 1]
             y [-1 0 1]
             z [-1 0 1]
             :when (not= [0 0 0] [x y z])]
         [x y z 0])))

(defn hyper-neighbors [p]
  (map #(map + p %)
       (for [x [-1 0 1]
             y [-1 0 1]
             z [-1 0 1]
             w [-1 0 1]
             :when (not= [0 0 0 0] [x y z w])]
         [x y z w])))

(defn range-of
  [coll]
  (when seq
    (let [a (reduce min coll)
          b (reduce max coll)]
      (range a (inc b)))))

(defn candidates
  [active]
  (->> (for [x (range-of (map #(nth % 0) active))
             y (range-of (map #(nth % 1) active))
             z (range-of (map #(nth % 2) active))]
         [x y z 0])
       (mapcat neighbors)
       set))

(defn hyper-candidates
  [active]
  (->> (for [x (range-of (map #(nth % 0) active))
             y (range-of (map #(nth % 1) active))
             z (range-of (map #(nth % 2) active))
             w (range-of (map #(nth % 3) active))]
         [x y z w])
       (mapcat hyper-neighbors)
       set))

(defn next-active?
  [neighbors active coord]
  (let [n (count (filter active (neighbors coord)))]
    (some? (if (active coord)
             (#{2 3} n)
             (#{3} n)))))

(defn step
  [{:keys [candidates neighbors]} active]
  (set (filter (partial next-active? neighbors active)
               (candidates active))))

(defn show-plane
  ([active z] (show-plane active z 0))
  ([active z w]
      (->>
       (for [x (range-of (map #(nth % 0) active))
             y (range-of (map #(nth % 1) active))]
         [x y z w])
       (sort-by second)
       (partition-by second)
       (map (fn [coll] (str/join (map (comp {true \# false \.} some? active) coll))))
       (str/join "\n"))))

(def f1 (partial step {:candidates candidates :neighbors neighbors}))

(->> example (iterate f1) (drop 6) first count)
(->> sky (iterate f1) (drop 6) first count)

(def f2 (partial step {:candidates hyper-candidates
                       :neighbors hyper-neighbors}))

(->> example (iterate f2) (drop 6) first count)
(->> sky (iterate f2) (drop 6) first count println)
