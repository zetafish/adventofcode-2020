(ns aoc.d10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (mapv #(Long/parseLong %)
                 (str/split-lines (slurp (io/resource "d10.txt")))))

(def example1 [16 10 15 5 1 11 7 19 6 12 4])
(def example2 [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])

(defn build-chain [coll]
  (let [coll (sort coll)]
    (vec (concat [0] coll [(+ 3 (last coll))]))))

(defn jolt-distribution
  [coll]
  (frequencies
   (map #(- %2 %1) coll (drop 1 coll))))

;; part 1
(jolt-distribution (build-chain example1))
(jolt-distribution (build-chain example2))
(reduce * (vals (jolt-distribution (build-chain input))))

(defn can-remove-at?
  [n coll]
  (<= (- (nth coll (inc n)) (nth coll (dec n))) 3))

(defn count-ways
  [coll]
  (loop [seen #{coll} todo [coll]]
    (if (empty? todo)
      (count seen)
      (let [coll (vec (first todo))
            candidates (->> (drop 1 (range (dec (count coll))))
                            (filter #(can-remove-at? % coll))
                            (map #(assoc coll % nil))
                            (map #(remove nil? %))
                            (remove seen))]
        (recur (into seen candidates)
               (into (rest todo) candidates))))))

(defn find-sub-chains
  [coll]
  (->> coll
       (map-indexed (fn [i x] [(and (< 0 i (dec (count coll))) (can-remove-at? i coll)) x]))
       (reduce (fn [{:keys [result segment inside prev] :as acc} [can-remove? current]]
                 (let [acc (assoc acc :inside can-remove? :prev current)]
                   (cond
                     ;; we are still inside a segment
                     (and inside can-remove?) (update acc :segment conj current)

                     ;; segment ends
                     inside (update acc :result conj (conj segment current))

                     ;; segment starts
                     can-remove? (assoc acc :segment [prev current])

                     ;; we are still outside
                     :else acc)))
               {})
       :result))

;; part 2
(->> example1 build-chain find-sub-chains (map count-ways) (reduce *))
(->> example2 build-chain find-sub-chains (map count-ways) (reduce *))
(->> input  build-chain find-sub-chains (map count-ways) (reduce *))
