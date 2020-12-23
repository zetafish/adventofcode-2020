(ns aoc.23
  (:require [clojure.string :as str]))

(def input [7 8 9 4 6 5 1 2 3])

(def cups [3 2 4 1 5])

(defn pick-up-cups
  [{:keys [circle] :as state}]
  (let [x (first circle)]
    (-> state
        (assoc :hand (take 3 (drop 1 circle)))
        (assoc :circle (concat [x] (drop 4 circle))))))

(defn find-destination
  [{:keys [hand circle] :as state}]
  (let [lowest (apply min (concat hand circle))
        highest (apply max (concat hand circle))]
    (loop [d (dec (first circle))]
      (cond
        (< d lowest) (recur highest)
        (some #{d} hand) (recur (dec d))
        :else (assoc state :destination d)))))

(defn place-cups
  [{:keys [hand circle destination] :as state}]
  (let [[a [_ & b]] (split-with #(not= destination %) circle)]
    (-> state
        (assoc :circle (concat a [destination] hand b))
        (dissoc :destination :hand))))

(defn bump-current
  [{circle :circle :as state}]
  (assoc state :circle (concat [(second circle)] (nnext circle) [(first circle)])))

(defn play-turn
  [state]
  (-> state
      pick-up-cups
      find-destination
      place-cups
      bump-current))

(defn announce
  [state]
  (let [[a [_ & b]] (split-with #(not= 1 %) (:circle state))]
    (str/join (concat b a))))

(->> {:circle [3 8 9 1 2 5 4 6 7]} (iterate play-turn) (drop 100) first announce)
(->> {:circle [7 8 9 4 6 5 1 2 3]} (iterate play-turn) (drop 100) first announce)
