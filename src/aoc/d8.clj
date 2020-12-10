(ns aoc.d8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse [s]
  (let [[cmd n] (str/split s #" ")]
    [(keyword cmd) (Integer/parseInt n)]))

(def input (mapv parse (str/split-lines (slurp (io/resource "d8.txt")))))

(def example (mapv parse ["nop +0"
                          "acc +1"
                          "jmp +4"
                          "acc +3"
                          "jmp -3"
                          "acc -99"
                          "acc +1"
                          "jmp -4"
                          "acc +6"]))

(defn step [{:keys [acc slots ip seen] :as state}]
  (let [[cmd n] (get slots ip)]
    (cond
      (:exit-code state) (assoc state :halted true)
      (seen ip) (assoc state :exit-code :loop-detected)
      (= ip (count slots)) (assoc state :exit-code :completed)
      :else (->
             (case cmd
               :nop (update state :ip inc)
               :jmp (update state :ip + n)
               :acc (-> state (update :ip inc) (update :acc + n)))
             (update :seen conj ip)))))

(defn run
  [slots]
  (->> {:acc 0 :slots slots :ip 0 :seen #{}}
       (iterate step)
       (take-while (complement :halted))
       last
       ((juxt :exit-code :acc))))

;; part 1
(run example)
(run input)

(defn swap [slots i]
  (case (first (slots i))
    :jmp (assoc-in slots [i 0] :nop)
    :nop (assoc-in slots [i 0] :jmp)
    slots))

(defn fix-bug
  [slots]
  (->> (range (count slots))
       (map #(swap slots %))
       (map run)
       (filter (comp #{:completed} first))))

(fix-bug example)
(fix-bug input)
