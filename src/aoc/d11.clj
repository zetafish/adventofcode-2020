(ns aoc.d11
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(set! *print-length* 10)

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

(def example2 (build-map [".......#."
                          "...#....."
                          ".#......."
                          "........."
                          "..#L....#"
                          "....#...."
                          "........."
                          "#........"
                          "...#....."]))

(def input (build-map (str/split-lines (slurp (io/resource "d11.txt")))))

(def dd (for [x [-1 0 1]
              y [-1 0 1]
              :when (not (and (zero? x) (zero? y)))]
          [x y]))

(defn cells [{:keys [mx my]}]
  (for [x (range mx)
        y (range my)]
    [x y]))

(defn valid-cell? [{:keys [mx my]} [x y]]
  (and (<= 0 x (dec mx)) (<= 0 y (dec my))))

(defn lookup [{grid :grid} [x y]]
  (get-in grid [y x]))

(defn update-cell [{grid :grid :as m} [x y] v]
  (assoc-in m [:grid y x] v))

(defn adj-scan
  "Scan for occupied seat"
  [m from rv]
  (let [seat (mapv + rv from)]
    (when (and (valid-cell? m seat)
               (= \# (lookup m seat)))
      seat)))

(defn beam-scan
  "Scan for occupied seat"
  [m from rv]
  (let [seat (->>
              from
              (iterate (partial mapv + rv))
              (drop 1)
              (take-while #(valid-cell? m %))
              (drop-while #(= \. (lookup m %)))
              first)]
    (when (= \# (lookup m seat))
      seat)))

(defn step
  [m {:keys [tolerance scan]}]
  (->> (cells m)
       (filter #(not= \. (lookup m %)))
       (map (fn [cell] {:cell cell
                        :occupied? (= \# (lookup m cell))
                        :scan (seq (keep (partial scan m cell) dd))}))
       (reduce (fn [m {:keys [cell occupied? scan]}]
                 (cond
                   (and (not occupied?) (empty? scan)) (update-cell m cell \#)
                   (and occupied? (>= (count scan) tolerance)) (update-cell m cell \L)
                   :else m))
               m)))

(defn stableize
  [m opts]
  (->> m
       (iterate #(step % opts))
       (partition 2)
       (take 100)
       (drop-while (fn [[a b]] (not= a b)))
       ffirst
       :grid
       (mapcat identity)
       frequencies))

;; part 1
(stableize example1 {:tolerance 4 :scan adj-scan})
(stableize input {:tolerance 4 :scan adj-scan})

;; part 2
(stableize example1 {:tolerance 5 :scan beam-scan})
(stableize input {:tolerance 5 :scan beam-scan})
