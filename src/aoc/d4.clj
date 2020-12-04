(ns aoc.d4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]))

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

(s/def ::byr string?) ; birth year
(s/def ::iyr string?) ; issue year
(s/def ::eyr string?) ; expiration year
(s/def ::hgt string?) ; height
(s/def ::hcl string?) ; hair color
(s/def ::ecl string?) ; eye color
(s/def ::pid string?) ; passport id
(s/def ::cid string?) ; country id
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

(->> input
     parse-batch
     (filter #(s/valid? ::passport %))
     count)
