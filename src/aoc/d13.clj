(ns aoc.d13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(set! *print-length* 20)

(defn parse-int [s]
  (try
    (biginteger (Integer/parseInt s))
    (catch Exception _)))

(def input (let [[s1 s2] (str/split-lines (slurp (io/resource "d13.txt")))]
             {:start (parse-int s1)
              :ids (map parse-int (str/split s2 #","))}))

(def example {:start 939
              :ids [7 13 nil nil 59 nil 31 19]})

(defn wait-for-bus
  [{:keys [start ids]}]
  (let [ids (filter some? ids)
        [id depart] (->> ids
                         (map (fn [k]
                                (->> (iterate #(+ k %) 0)
                                     (drop-while #(< % start))
                                     first)))
                         (zipmap ids)
                         (reduce (fn [res x]
                                   (if (< (second res) (second x))
                                     res
                                     x))))]
    {:id id :depart depart :wait (- depart start)}))

;; part 1
(->> (wait-for-bus example) ((juxt :id :wait)) (reduce *))
(->> (wait-for-bus input) ((juxt :id :wait)) (reduce *))

(defn gcd [a b]
  (let [a (BigInteger/valueOf a)
        b (BigInteger/valueOf b)]
    (.gcd a b)))

(defn lcm [a b]
  (let [g (gcd a b)]
    (/ (* a b) g)))

(defn coprime?
  [a b]
  (= 1 (gcd a b)))

(reduce lcm (filter some? (:ids example)))

(reduce lcm (filter some? (:ids input)))

(defn coefs
  [ids]
  (->> (map-indexed vector ids)
       (filter second)
       (map (fn [[a n]] [(mod (- n a) n) n]))))

(defn xgcd
  "Extended Euclidean Algorithm. Returns [gcd(a,b) x y] where ax + by = gcd(a,b)."
  [a b]
  (if (= a 0)
    [b 0 1]
    (let [[g x y] (xgcd (mod b a) a)]
      [g (- y (* (quot b a) x)) x])))

(defn chinese-remainder
  "Find x such that x==a1 (mod n1) and x==a2 (mod n2) where n1 and n2
  are coprime.

  See https://en.wikipedia.org/wiki/Chinese_remainder_theorem"
  [[a1 n1] [a2 n2]]
  (let [[_ m1 m2] (xgcd n1 n2)]
    (mod (+ (* a1 m2 n2) (* a2 m1 n1))
         (* n1 n2))))

(defn chinese-remainder*
  [coll]
  (loop [[[_ n1 :as p] [_ n2 :as q] & more] coll]
    (let [z (chinese-remainder p q)]
      (if (empty? more)
        z
        (recur (conj more [z (* n1 n2)]))))))


(chinese-remainder* (coefs (:ids example)))
(chinese-remainder* (coefs [17 nil 13 19]))
(chinese-remainder* (coefs [67 7 59 61]))
(chinese-remainder* (coefs [67 nil 7 59 61]))
(chinese-remainder* (coefs [67 7 nil 59 61]))
(chinese-remainder* (coefs [1789 37 47 1889]))
(chinese-remainder* (coefs (:ids input)))
