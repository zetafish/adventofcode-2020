(ns aoc.d16
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse-field [s]
  (let [[_ n & nums] (re-matches #"([^:]+): (\d+)-(\d+) or (\d+)-(\d+)" s)
        [a b c d :as numbers] (map parse-int nums)]
    {:name n
     :numbers numbers
     :valid? (fn [k] (or (<= a k b) (<= c k d)))}))

(defn parse-ticket [s]
  (map parse-int (str/split s #",")))

(def data (let [s (str/split-lines (slurp (io/resource "d16.txt")))
                 [fields my nearby] (remove #(= [""] %) (partition-by str/blank? s))]
             {:fields (map parse-field fields)
              :my (parse-ticket (second my))
              :nearby (map parse-ticket (drop 1 nearby))}))

(def fields (:fields data))
(def n-fields (count (:fields data)))
(def my (:my data))
(def nearby (:nearby data))

(defn field-valid?
  [n]
  (some  #((:valid? %) n) (:fields data)))

;; part 1
(reduce + (remove field-valid? (apply concat (:nearby data))))

(def valid-tickets (filter (partial every? field-valid?) (:nearby data)))

(defn candidate-positions
  [{:keys [valid?]}]
  (->> (range n-fields)
       (map (fn [i] [i (every? valid? (map #(nth % i) valid-tickets))]))
       (filter second)
       (map first)))

(def assignments
  (loop [m {}
         seen #{}
         candidates (->> fields
                         (map candidate-positions)
                         (zipmap fields)
                         (sort-by (comp count second)))]
    (if (empty? candidates)
      m
      (let [[f c] (first candidates)
            c (remove seen c)]
        (assert (= 1 (count c)))
        (recur (assoc m f (first c)) (conj seen (first c)) (rest candidates))))))

(->> fields
     (filter #(str/starts-with? (:name %) "departure"))
     (map assignments)
     (map #(nth my %))
     (reduce *)
     (println))
