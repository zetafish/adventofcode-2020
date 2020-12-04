(ns aoc.d4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]))

(def example ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
              "byr:1937 iyr:2017 cid:147 hgt:183cm"
              ""
              "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
              "hcl:#cfa07d byr:1929"
              ""
              "hcl:#ae17e1 iyr:2013"
              "eyr:2024"
              "ecl:brn pid:760753108 byr:1931"
              "hgt:179cm"
              ""
              "hcl:#cfa07d eyr:2025 pid:166559648"
              "iyr:2011 ecl:brn hgt:59in"])

(def input (str/split-lines (slurp (io/resource "d4.txt"))))

(defn number-field?
  ([s min max] (number-field? s "" min max))
  ([s suffix min max]
   (let [pat (re-pattern (str "(\\d+)" suffix))]
     (when-let [x (last (re-matches pat s))]
       (<= min (Integer/parseInt x) max)))))


(s/def ::byr #(number-field? % 1920 2002))
(s/def ::iyr #(number-field? % 2010 2020))
(s/def ::eyr #(number-field? % 2020 2030))
(s/def ::hgt (s/or :cm #(number-field? % "cm" 150 193)
                   :in #(number-field? % "in" 59 76)))
(s/def ::hcl #(re-matches #"#[0-9a-f]{6}" %))
(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::pid #(re-matches #"[0-9]{9}" %))
(s/def ::cid string?)
(s/def ::passport (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                          :opt-in [::cid]))

(defn parse-field
  [kv]
  (let [[k v] (str/split kv #":")]
    [(keyword k) v]))

(defn parse-batch
  [batch]
  (->> batch
       (partition-by str/blank?)
       (map (partial str/join " "))
       (remove str/blank?)
       (map #(str/split % #" "))
       (map (fn [kvs] (into {} (map parse-field kvs))))))

(defn valid-1?
  [m]
  (every? (set (keys m)) [:byr :iyr :eyr :hgt :hcl :ecl :pid]))

(defn valid-2?
  [m]
  (s/valid? ::passport m))

;; part 1
(->> example parse-batch (filter valid-1?) count)
(->> input parse-batch (filter valid-1?) count)

;; part 2
(->> example parse-batch (filter valid-2?) count)
(->> input parse-batch (filter valid-2?) count)
