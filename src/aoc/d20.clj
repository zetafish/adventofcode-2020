(ns aoc.d20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def N 10)

(def empty-grid (vec (repeat N (vec (repeat N 0)))))

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

(def coords (for [x (range N) y (range N)] [x y]))

(defn grid->str [g]
  (str/join "\n" (map str/join g)))

(defn get-pixel [g [x y]]
  (get-in g [y x]))

(defn set-pixel [g [x y] v]
  (assoc-in g [y x] v))

(defn transform
  [fp g]
  {:pre [(every? #(every? some? %) g)]}
  (reduce (fn [result p]
            (let [v (get-pixel g p)]
              (set-pixel result (fp p) v)))
          empty-grid
          coords))

(def flip-main-diagonal (partial transform (fn [[x y]] [y x])))
(def flip-other-diagonal (partial transform (fn [[x y]] [(- N y 1) (- N x 1)])))
(def flip-vertical-axis (partial transform (fn [[x y]] [(- N x 1) y])))
(def flip-horizontal-axis (partial transform (fn [[x y]] [x (- N y 1)])))

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

(defn side [direction grid]
  (case direction
    :north (first grid)
    :south (last grid)
    :west (map first grid)
    :east (map last grid)))

(defn all-sides [grid]
  (let [sides (map #(side % grid) directions)]
    (set (concat sides (map reverse sides)))))

(defn build-world [m size]
  (let [sides (zipmap (map first m)
                      (map all-sides (map second m)))
        combos (for [a (keys m) b (keys m)
                     :when (and (not= a b)
                                (seq (set/intersection (sides a)
                                                       (sides b))))]
                 [a b])
        graph (reduce (fn [acc [a b]]
                        (update acc a conj b))
                      {}
                      combos)]
    {:tiles m
     :graph graph
     :floor {:size size :layout {} :where {}}}))

(defn corner-tiles [{:keys [graph]}]
  (map first (filter #(= 2 (count (second %))) graph)))

(defn spec-one-side [{{:keys [layout size]} :floor} [x y] direction]
  (when-let [r (case direction
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
    [direction r]))

(defn spec-all-around [world point]
  (keep #(spec-one-side world point %) directions))

(defn fit-as-is? [from direction to]
  (= (side direction from)
     (side (opposite direction) to)))

(defn border? [from direction others]
  (->> others
       (mapcat orientations)
       (filter #(fit-as-is? from direction %))
       empty?))

(defn border-directions
  [{:keys [tiles graph]} id tile]
  (let [others (map tiles (graph id))]
    (filter #(border? tile % others) directions)))

(defn satisfies-spec?
  [world spec id tile]
  (reduce (fn [acc [dir pattern]]
            (cond
              (not acc) false

              (= :border pattern)
              (contains? (set (border-directions world id tile)) dir)

              (nil? pattern)
              true

              :else
              (= pattern (side dir tile))))
          true
          spec))

(defn orientate-to-spec [world spec id]
  (let [tiles (filter #(satisfies-spec? world spec id %)
                      (orientations (get-in world [:tiles id])))]
    (when (> (count tiles) 1)
      (println "found" (count tiles) "orientations for" id))
    (first tiles)))

(defn find-candidates [{:keys [graph floor]} point]
  (if (= point [0 0])
    (map first (filter #(= 2 (count (second %))) graph))
    (->> [[0 1] [1 0] [0 -1] [-1 0]]
         (keep (comp graph
                     (:ids floor)
                     vec
                     #(map + point %)))
         (map set)
         (reduce set/intersection)
         vec)))

(defn put-next-tile [world point]
  ;;#dbg ^{:break/when (= [0 9] point)}
  (let [spec (spec-all-around world point)
        candidates (->> (find-candidates world point)
                       (map #(vector % (orientate-to-spec world spec %)))
                       (filter second)
                       doall)
        [id tile] (first candidates)]
    (println point "=>" (map first candidates))
    (-> world
        (assoc-in [:floor :ids point] id)
        (assoc-in [:floor :layout point] tile)
        (assoc-in [:floor :where id] point))))


(time
 (let [n 3
       w (build-world example n)]
   (println "---")
   (-> (reduce put-next-tile w
               (for [x (range n) y (range n)] [x y]))
       :floor :ids)))

#_(let [n 10
      w (build-world input n)]
  (println "--")
  (-> (reduce put-next-tile w
              (for [x (range n) y (range n)] [x y]))
      :floor :ids))

(def big (build-world input 10))

(def b2 (reduce put-next-tile big (for [y (range 9)] [0 y])))

(defn show-adj [id]
  (println id "=>" (get-in big [:graph id])))

(defn traverse [{:keys [graph]} corner depth]
  (loop [ring [corner] depth depth seen #{}]
    (if (zero? depth)
      (sort ring)
      (recur (distinct (remove seen (mapcat graph ring)))
             (dec depth)
             (into seen ring)))))


(defn visit-all [{:keys [graph]} corner]
  (loop [n 0 visited [] todo [corner] seen #{corner}]
    (println n visited todo)
    (if (empty? todo)
      visited
      (let [p (first todo)
            c (remove seen (graph p))]
        (recur (inc n)
               (conj visited p)
               (into (rest todo) c)
               (into seen c))))))

(first (corner-tiles w))
(visit-all w 1171)

(first (corner-tiles big))
(visit-all big 1549)



(println (sort (corner-tiles big)))
(println (traverse (:graph big) 3539 9))
(get-in big [:graph 3329])

(set/intersection (set (corner-tiles big))
                  (set (traverse big 1549 11)))

(show-adj 1549)
(show-adj 3779)


(println "[0 9] candidates" (get-in b2 [:graph 3061]))

(println (str "\nTile [0 8]\n"
              (grid->str (get-in b2 [:floor :layout [0 8]]))))

(defn show-floor-tile [world point]
  (let [t (get-in world [:floor :layout point])
        id (get-in world [:floor :ids point])
        borders (vec (border-directions world id t))]
    (str "\nTile " point
         "\nId " id
         "\nBorders " borders
         "\n" (grid->str (get-in world [:floor :layout point])))))

(def s (get-in b2 [:floor :layout [0 8]]))
(def t (get-in b2 [:tiles 2347]))

(println (show-floor-tile b2 [0 0]))
(println (show-floor-tile b2 [0 1]))
(println (show-floor-tile b2 [0 2]))
(println (show-floor-tile b2 [0 3]))
(println (show-floor-tile b2 [0 4]))
(println (show-floor-tile b2 [0 5]))
(println (show-floor-tile b2 [0 6]))
(println (show-floor-tile b2 [0 7]))
(println (show-floor-tile b2 [0 8]))
(println (show-floor-tile b2 [0 9]))


(println (:ids (:floor b2)))

(spec-all-around b2 [0 9])
(border-directions b2 2347 t)
(get-in b2 [:graph ])

(reduce * (corner-tiles (build-world example 3)))
(reduce * (corner-tiles (build-world input 10)))

;; corners: (1549 3709 2693 3539)
(get-in b2 [:graph 1549])

(println "\n")
(println (grid->str (get-in b2 [:tiles 1297])))

(defn build-graph-2 [m]
  )


(run! println (map str/join (all-sides (input 1549))))

(println "-") (println (grid->str (input 1549)))
