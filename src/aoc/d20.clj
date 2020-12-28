(ns aoc.d20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def monster (mapv vec ["                  # "
                        "#    ##    ##    ###"
                        " #  #  #  #  #  #   "]))

(defn parse-tile
  [[header & grid]]
  {(Integer/parseInt (get (re-matches #"Tile (\d+):" header) 1))
   (vec (map vec grid))})

(defn parse-input
  [s]
  (->> (str/split-lines (slurp (io/resource s)))
       (partition-by str/blank?)
       (remove #(= [""] %))
       (mapv parse-tile)
       (into {})))

(def example (parse-input "d20-example.txt"))

(def input (parse-input "d20.txt"))

(def directions [:east :north :west :south])

(def opposite {:east :west
               :west :east
               :north :south
               :south :north})

(defn grid->str [g]
  (str/join "\n" (map str/join g)))

(defn get-pixel [g [x y]]
  (get-in g [y x]))

(defn set-pixel [g [x y] v]
  (assoc-in g [y x] v))

(defn transform
  [fp g]
  {:pre [(every? #(every? some? %) g)]}
  (let [n (count g)]
    (reduce (fn [result p]
              (let [v (get-pixel g p)]
                (set-pixel result (fp p) v)))
            (vec (repeat n (vec (repeat n nil))))
            (for [x (range n) y (range n)] [x y]))))

(defn flip-vertical-axis [g]
  (mapv (comp vec reverse) g))

(defn flip-horizontal-axis [g]
  (vec (reverse g)))

(defn flip-main-diagonal [g]
  (transform (fn [[x y]] [y x]) g))

(defn flip-other-diagonal [g]
  (let [n (count g)]
    (transform (fn [[x y]] [(- n y 1) (- n x 1)]) g)))

(defn orientations
  "Square has 8 symmetries.

  See: http://facstaff.cbu.edu/wschrein/media/M402%20Notes/M402C1.pdf
  "
  [g]
  (distinct (map (fn [f] (f g))
                 [identity
                  flip-horizontal-axis
                  flip-vertical-axis
                  flip-main-diagonal
                  flip-other-diagonal
                  (comp flip-horizontal-axis flip-vertical-axis)
                  (comp flip-horizontal-axis flip-main-diagonal)
                  (comp flip-horizontal-axis flip-other-diagonal)])))

(defn show-plan [{:keys [plan size]}]
  (doseq [y (range size)]
    (doseq [x (range size)]
      (print (format "%6s" (get plan [x y]))))
    (println)))

(defn side [direction grid]
  (seq (case direction
         :north (first grid)
         :south (last grid)
         :west (map first grid)
         :east (map last grid))))

(defn all-sides [grid]
  (let [sides (map #(side % grid) directions)]
    (concat sides (map reverse sides))))

(defn build-world [m]
  (let [combos (for [a (keys m) b (keys m)
                     :when (and (not= a b)
                                (seq (set/intersection
                                      (set (all-sides (m a)))
                                      (set (all-sides (m b))))))]
                 [a b])
        graph (reduce (fn [acc [a b]]
                        (update acc a conj b))
                      {}
                      combos)
        size (int (Math/sqrt (count m)))]
    {:tiles m
     :graph graph
     :size size
     :histo (frequencies (mapcat all-sides (vals m)))
     :floor {:size size :layout {} :where {}}}))

(defn corner-tiles [{:keys [graph]}]
  (map first (filter #(= 2 (count (second %))) graph)))

(defn spec-one-side [{:keys [layout size]} [x y] direction]
  (let [r (case direction
            :north (if (zero? y)
                     :border
                     (side :south (get layout [x (dec y)])))
            :south (if (= (dec size) y)
                     :border
                     (side :north (get layout [x (inc y)])))
            :west (if (zero? x)
                    :border
                    (side :east (get layout [(dec x) y])))
            :east (if (= (dec size) x)
                    :border
                    (side :west (get layout [(inc x) y]))))]
    (when r
      [direction r])))

(defn spec-all-around [world point]
  (into {} (keep #(spec-one-side world point %) directions)))

(defn resolve-sides [{:keys [histo]} grid]
  (let [sides (map #(side % grid) directions)]
    (zipmap directions (map (fn [v]
                              (if (= 1 (histo v))
                                :border
                                v))
                            sides))))

(defn satisfies-spec? [world spec grid]
  (let [sides (resolve-sides world grid)]
    (every? #(or (nil? (spec %))
                 (= (spec %) (sides %)))
            directions)))

(defn orientate-to-spec [world spec id]
  (let [tiles (filter #(satisfies-spec? world spec %)
                      (orientations (get-in world [:tiles id])))]
    (first tiles)))

(defn neighbor-connected-tiles
  "Determine the tile ids at `point` based on what the neightbors have
  to say about it."
  [{:keys [plan graph]} point]
  (->> [[0 1] [1 0] [0 -1] [-1 0]]
       (map #(map + point %))
       (keep plan)
       (map (comp set graph))
       (apply set/intersection)))

(defn play-tile [{:keys [graph tiles plan size] :as world} point]
  (let [spec (spec-all-around world point)
        candidates (if (= point [0 0])
                     (take 1 (corner-tiles world))
                     (neighbor-connected-tiles world point))
        [id tile] (->> candidates
                       (map #(vector % (orientate-to-spec world spec %)))
                       (filter second)
                       first)]
    (-> world
        (assoc-in [:plan point] id)
        (assoc-in [:layout point] tile))))

(defn play-all-tiles [world]
  (reduce play-tile world
          (for [x (range (:size world))
                y (range (:size world))]
            [x y])))

(defn make-sea
  ([world] (uber-tile world true))
  ([{:keys [size layout]} cut-border?]
   (let [pixels-per-line (* size 10)
         pixels (for [y (range pixels-per-line)
                      x (range pixels-per-line)
                      :when (or (not cut-border?) (and (< 0 (rem x 10) 9)
                                                       (< 0 (rem y 10) 9)))]
                  (let [t [(quot x 10) (quot y 10)]
                        s [(rem x 10) (rem y 10)]]
                    (get-pixel (get layout t) s)))
         n (if cut-border? 8 10)]
     (mapv vec (partition (* n size) pixels)))))

(defn black-pixels [g]
  (for [x (range (count (first g)))
        y (range (count g))
        :when (= \# (get-in g [y x]))]
    [x y]))

(defn find-monsters [tile]
  (let [nx (- (count (first tile)) (count (first monster)))
        ny (- (count tile) (count monster))
        tile (set (black-pixels tile))
        monster (black-pixels monster)
        k (count monster)]
    (->>
     (for [x (range nx) y (range ny)] [x y])
     (filter (fn [v] (= k (count
                           (set/intersection
                            tile
                            (set (map #(map + v %) monster)))))))
     seq)))

(defn compute-roughness [sea]
  (let [n (count (first (keep find-monsters (orientations sea))))]
    (- (count (black-pixels sea))
       (* n (count (black-pixels monster))))))

;; part 1
(reduce * (corner-tiles (build-world example)))
(reduce * (corner-tiles (build-world input)))

;; part 2
(-> example build-world play-all-tiles make-sea compute-roughness)
(-> input build-world play-all-tiles make-sea compute-roughness)
