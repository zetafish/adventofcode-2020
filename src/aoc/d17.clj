(ns aoc.d17
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (str/split-lines (slurp (io/resource "d17.txt"))))

(defn build-sky
  [plane]
  (let [plane (mapv vec plane)
        max-x (count (first plane))
        max-y (count plane)]
    {:active (set (for [x (range max-x)
                        y (range max-y)
                        :when (= \# (get-in plane [y x]))]
                    [x y 0]))}))

(def sky (build-sky input))

(def example (build-sky [".#." "..#" "###"]))

(def d (for [dx [-1 0 1]
             dy [-1 0 1]
             dz [-1 0 1]
             :when (not= [dx dy dz] [0 0 0])]
         [dx dy dz]))

(defn plane
  [{active :active} z]
  (filter #(= z (get % 2)) active))

(defn empty-plane?
  [sky z]
  (zero? (count (plane sky z))))

(defn neighbors [[x y z]]
  (map #(map + [x y z] %) d))

(defn range-of
  [coll]
  (when seq
    (let [a (reduce min coll)
          b (reduce max coll)]
      (range a (inc b)))))


(defn candidates
  [{active :active}]
  (->> (for [x (range-of (map #(nth % 0) active))
             y (range-of (map #(nth % 1) active))
             z (range-of (map #(nth % 2) active))]
         [x y z])
       (mapcat neighbors)
       set))

(defn next-active?
  [{active :active} coord]
  (let [n (count (filter active (neighbors coord)))]
    (if (active coord)
      (#{2 3} n)
      (#{3} n))))


(defn step
  [sky]
  (let [cs (candidates sky)
        survivors (filter (partial next-active? sky) cs)]
    (assoc sky :active (set survivors))))

(defn show-plane
  [{active :active} z]
  (->>
   (for [x (range-of (map #(nth % 0) active))
         y (range-of (map #(nth % 1) active))]
     [x y z])
   (sort-by second)
   (partition-by second)
   (map (fn [coll] (str/join (map (comp {true \# false \.} some? active) coll))))
   (str/join "\n")))


(->> (iterate step example)
     (drop 6)
     first
     :active count)

(->> (iterate step sky)
     (drop 6)
     first :active count)
(show-plane (first (drop 5(iterate step example))))
