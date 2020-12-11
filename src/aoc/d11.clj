(ns aoc.d11
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn build-map [lines]
  (let [grid (mapv vec lines)]
    {:grid grid
     :mx (count (first grid))
     :my (count grid)}))

(def example1 (build-map ["L.LL.LL.LL"
                          "LLLLLLL.LL"
                          "L.L.L..L.."
                          "LLLL.LL.LL"
                          "L.LL.LL.LL"
                          "L.LLLLL.LL"
                          "..L.L....."
                          "LLLLLLLLLL"
                          "L.LLLLLL.L"
                          "L.LLLLL.LL"]))

(def input (build-map (str/split-lines (slurp (io/resource "d11.txt")))))

(def adj (for [x [-1 0 1]
               y [-1 0 1]
               :when (not (and (zero? x) (zero? y)))]
           [x y]))

(defn cells [{:keys [mx my]}]
  (for [x (range mx)
        y (range my)]
    [x y]))

(defn around [{:keys [grid mx my]} [x y]]
  (->> adj
       (map (partial map + [x y]))
       (filter (fn [[x y]] (and (<= 0 x (dec mx)) (<= 0 y (dec my)))))))

(defn lookup [{grid :grid} [x y]]
  (get-in grid [y x]))

(defn update-cell [{grid :grid :as m} [x y] v]
  (assoc-in m [:grid y x] v))

(defn step [m]
  (->> (cells m)
       (filter #(not= \. (lookup m %)))
       (map (fn [cell] {:cell cell
                        :nextval (let [v (lookup m cell)
                                       n (->> (around m cell)
                                              (filter #(= \# (lookup m %)))
                                              count)]
                                   (cond
                                     (and (zero? n) (= \L v)) \#
                                     (and (>= n 4) (= \# v)) \L
                                     :else v))}))
       (reduce (fn [m {:keys [cell nextval]}]
                 (update-cell m cell nextval))
               m)))

(defn stableize
  [m]
  (->> m
       (iterate step)
       (partition 2)
       (take 100)
       (drop-while (fn [[a b]] (not= a b)))
       ffirst
       :grid
       (mapcat identity)
       frequencies))

;; part 1
(stableize example1)
(stableize input)
