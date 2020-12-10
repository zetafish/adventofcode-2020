(ns aoc.d8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (str/split-lines (slurp (io/resource "d8.txt"))))

(def example ["nop +0"
              "acc +1"
              "jmp +4"
              "acc +3"
              "jmp -3"
              "acc -99"
              "acc +1"
              "jmp -4"
              "acc +6"])

(defn parse [s]
  (let [[cmd n] (str/split s #" ")]
    [(keyword cmd) (Integer/parseInt n)]))

(map parse input)

(map parse example)

(defn step [{:keys [acc slots ip seen halted] :as state}]
  (let [[cmd n] (get slots ip)]
    (if (seen ip)
      (assoc state :halted true)
      (->
       (case cmd
         :nop (update state :ip inc)
         :jmp (update state :ip + n)
         :acc (-> state
                  (update :ip inc)
                  (update :acc + n)))
       (update :seen conj ip)))))

(defn run
  [slots]
  (iterate step {:acc 0 :slots slots :ip 0 :seen #{}}))

;; part 1
(->> (mapv parse example) run (take-while (complement :halted)) last :acc)
(->> (mapv parse input) run (take-while (complement :halted)) last :acc)
