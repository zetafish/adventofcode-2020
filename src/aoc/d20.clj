(ns aoc.d20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def N 10)

(defn parse-tile
  [[header & grid]]
  {:id (Integer/parseInt (get (re-matches #"Tile (\d+):" header) 1))
   :grid (let [g (vec (map vec grid))]
           g
           #_(->> (for [x (range N) y (range N)] [y x])
                (map #(get-in g %))
                (into {})))})

(defn parse-input
  [s]
  (let [tiles (->> (str/split-lines (slurp (io/resource s)))
                   (partition-by str/blank?)
                   (remove #(= [""] %))
                   (mapv parse-tile))]
    {:tiles tiles
     :ids (map :id tiles)
     :grids (map :grid tiles)
     :by-id (zipmap (map :id tiles) tiles)}))

(def example (parse-input "d20-example.txt"))

(def input (parse-input "d20.txt"))

(def opposite {:east :west
               :west :east
               :north :south
               :south :north})

(def coords (for [x (range N) y (range N)] [x y]))

(defn grid->str [g]
  (str/join "\n" (map str/join g)))

(defn print-grid [g]
  (println "---")
  (println ))

(defn get-pixel [grid [x y]]
  (get-in grid [y x]))

(defn set-pixel [grid [x y] v]
  (assoc-in grid [y x] v))

(defn x-flip [grid]
  (mapv (comp vec reverse) grid))

(defn y-flip [grid]
  (reverse grid))

(defn rot-coord [[x y]]
  (-> [x y]
      ((fn [[x y]] [(- x 4.5) (- y 4.5)]))
      ((fn [[x y]] [(- y) x]))
      ((fn [[x y]] [(+ x 4.5) (+ y 4.5)]))
      ((fn [[x y]] [(int x) (int y)]))))

(defn rot [grid]
  (reduce (fn [result [x y]]
            (set-pixel result (rot-coord [x y])
                       (get-pixel grid [x y])))
          (vec (repeat N (vec (repeat N \.))))
          coords))

(defn orientations
  [g]
  (set (reductions (fn [g f] (f g))
                   g [rot rot rot rot
                      x-flip rot rot rot rot
                      y-flip rot rot rot rot])))

(orientations g)

(defn neighbor [side coord]
  (let [d {:east [1 0] :west [-1 0] :north [-1 0] :south [1 0]}]
    (map + coord (d side))))

(defn edge [side grid]
  (->> (case side
         :west  (map #(vector 0 %) (range N))
         :east  (map #(vector (dec N) %) (range N))
         :north (map #(vector % 0) (range N))
         :south (map #(vector % (dec N)) (range N)))
       (map #(get-pixel grid %))))

(defn all-edges [grid]
  (map #(edge % grid) [:east :north :west :south]))

(defn fits-edge-grid? [edge grid]
  (let [ee (set (all-edges grid))]
    (or (contains? ee edge)
        (contains? ee (reverse edge)))))

(defn outer-edge? [edge grids]
  (not (some #(fits-edge-grid? edge %) grids)))

(defn outer-edge-count [grids g]
  (count (filter #(outer-edge? % (remove #{g} grids)) (all-edges g))))

(defn corner-tiles
  [data]
  (->> (map (fn [t] {:id (:id t)
                     :outers (outer-edge-count (:grids data) (:grid t))})
            (:tiles data))
       (filter #(= 2 (:outers %)))))
