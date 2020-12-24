(ns aoc.d24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

;; https://www.redblobgames.com/grids/hexagons/

(defn parse-route
  [s]
  (:route
   (reduce (fn [acc ch]
             (case ch
               \n (assoc acc :north-south \n)
               \s (assoc acc :north-south \s)
               (let [seg  (str (:north-south acc) ch)]
                 (-> acc
                     (dissoc :north-south)
                     (update :route (fnil conj []) seg)))
               ))
           {}
           s)))

(def input (map parse-route
                (str/split-lines
                 (slurp (io/resource "d24.txt")))))

(def example (map parse-route
                  (str/split-lines
                   (slurp (io/resource "d24-example.txt")))))

(def d-even {"e"  [1 0]
             "w"  [-1 0]
             "se" [0 1]
             "sw" [-1 1]
             "ne" [0 -1]
             "nw" [-1 -1]})

(def d-odd {"e"  [1 0]
            "w"  [-1 0]
            "se" [1 1]
            "sw" [0 1]
            "ne" [1 -1]
            "nw" [0 -1]})

(defn step [[_ y :as point] d]
  (cond
    (even? y) (mapv + point (d-even d))
    (odd? y)  (mapv + point (d-odd d))))

(defn follow-plan [routes]
  (->> (map #(reduce step [0 0] %) routes)
       frequencies
       (filter (comp odd? second))
       (map first)
       set))

(defn neighbors [[_ y :as point]]
  (map #(mapv + point %)
       (vals (if (even? y) d-even d-odd))))

(defn next-gen [black-tiles]
  (let [all-tiles (set (concat black-tiles (mapcat neighbors black-tiles)))
        white-tiles (set/difference all-tiles black-tiles)]
    (->> all-tiles
         (filter (fn [p]
                  (let [n (->> (neighbors p)
                               (filter black-tiles)
                               count)]
                    (or (and (black-tiles p) (#{1 2} n))
                        (and (white-tiles p) (#{2} n))))))
         set)))

(defn generate [black-tiles days]
  (->> black-tiles
       (iterate next-gen)
       (drop 1)
       (take days)
       last))


;; part 1
(count (follow-plan example))
(count (follow-plan input))

;; part 2
(-> example follow-plan (generate 100) count)
(-> input follow-plan (generate 100) count)
