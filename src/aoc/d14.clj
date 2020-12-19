(ns aoc.d14
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def initial (str/join (repeat 36 "0")))

(defn int->bits
  [v]
  (let [b (Integer/toBinaryString v)
        prefix (str/join (repeat (- 36 (count b)) "0"))]
    (str prefix b)))

(defn bits->int
  [bits]
  (Long/parseLong bits 2))

(defn p-mask [s]
  (let [mask (subs s (count "mask = "))]
    {:op :mask
     :mask mask}))

(defn p-mem [s]
  (let [[_ k v] (re-matches #"mem\[(\d+)\] = (\d+)" s)
        k (Integer/parseInt k)
        v (Integer/parseInt v)]
    {:op :mem
     :slot k
     :bits (int->bits v)}))

(defn parse [s]
  (cond
    (str/starts-with? s "mask") (p-mask s)
    (str/starts-with? s "mem") (p-mem s)))

(def input (map parse (str/split-lines (slurp (io/resource "d14.txt")))))

(def example-1 (map parse ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                           "mem[8] = 11"
                           "mem[7] = 101"
                           "mem[8] = 0"]))


(defn apply-mask
  [mask bits]
  (str/join (map (fn [m b]
                   (case m \X b \0 \0 \1 \1))
                 mask bits)))

(defmulti step (fn [_state {op :op}] op))

(defmethod step :mask
  [state cmd]
  (assoc state :mask (:mask cmd)))

(defmethod step :mem
  [state {:keys [slot bits]}]
  (assoc-in state [:slots slot] (apply-mask (:mask state) bits)))


(-> {}
    (step {:op :mask :mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"})
    (step {:op :mem :slot 8 :bits (int->bits 11)})
    (step {:op :mem :slot 7 :bits (int->bits 101)})
    (step {:op :mem :slot 8 :bits (int->bits 0)})
    :slots)

(->> example-1 (reduce step) :slots vals (map bits->int) (reduce +))
(->> input (reduce step) :slots vals (map bits->int) (reduce +))
