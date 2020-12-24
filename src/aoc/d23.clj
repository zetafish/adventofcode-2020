(ns aoc.d23
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]))


(defn make-state
  ([coll] (make-state coll (count coll)))
  ([coll n]
   (let [v (concat coll (range (inc (apply max coll)) (inc n)))]
     {:move 1
      :size n
      :current (first v)
      :value-of (vec v)
      :index-of (vec (concat [nil] (vals (into (sorted-map)
                                               (zipmap v (iterate inc 0))))))})))

(def example [3 8 9 1 2 5 4 6 7])
(def input [7 8 9 4 6 5 1 2 3])

(defn set-value [state index value]
  (-> state
      (assoc-in [:value-of (mod index (:size state))] value)
      (assoc-in [:index-of value] (mod index (:size state)))))

(defn get-value [{:keys [value-of size]} index]
  (get value-of (mod index size)))

(defn copy [{:keys [size value-of] :as state} from to]
  (let [from (mod from size)
        to (mod to size)]
    (set-value state to (value-of from))))

(defn str-state
  [{:keys [current value-of hand destination move]}]
  (str (str/join " " (map (fn [v]
                            (cond
                              (= current v) (str "(" v ")")
                              :else v))
                          value-of))
       ", move=" move
       ", hand=" hand
       ", dest=" destination)
  )

(defn block-after
  [{:keys [index-of size]} after]
  (map #(mod (+ (index-of after) %) size) [1 2 3]))

(defn select-hand
  [{:keys [value-of current] :as state}]
  (assoc state :hand (mapv value-of (block-after state current))))

(defn select-destination
  [{:keys [current hand size] :as state}]
  (assoc state :destination (loop [d (dec current)]
                              (cond
                                (some #{d} hand) (recur (dec d))
                                (< d 1) (recur size)
                                :else d))))

(defn shift-values
  [{:keys [hand destination index-of size] :as state}]
  (let [hand-idx (index-of (first hand))
        dest-idx (index-of destination)
        items-between (mod (- dest-idx hand-idx 3) size)
        state (reduce (fn [state n]
                        (set-value state
                                   (+ hand-idx n)
                                   (get-value state (+ hand-idx n 3))))
                      state
                      (range (inc items-between)))
        state (reduce (fn [state i]
                        (let [i (+ i hand-idx 1 items-between)]
                          (assoc-in state [:value-of (mod i size)] "_")))
                      state
                      (range 3))]
    state))

(defn put-down-hand
  [{:keys [hand index-of destination] :as state}]
  (let [offset (inc (index-of destination))]
    (reduce (fn [state i]
              (set-value state (+ offset i) (get hand i)))
            state
            (range 3))))

(defn bump
  [{:keys [index-of current] :as state}]
  (let [new-current (get-value state (inc (index-of current)))]
    (-> state
        (update :move inc)
        (dissoc :hand :destination)
        (assoc :current new-current))))

(defn move
  [state]
  (-> state
      select-hand
      select-destination
      shift-values
      put-down-hand
      bump))

(defn play-game [state n]
  (->> (iterate move state)
       (drop n)
       first))

(defn announce [state]
  (let [one (get-in state [:index-of 1])]
    (->> (range (:size state))
         (map #(+ one %))
         (drop 1)
         str/join)))


;; part 1
(play-game (make-state example) 100)
(announce (play-game (make-state input) 100))
