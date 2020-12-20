(ns aoc.d19
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [instaparse.core :as insta]))

(defn build-grammar [rules]
  (->> rules
       (map #(str/replace % #":" "="))
       (str/join "\n")))

(defn build-data [lines]
  (let [[rules _ messages] (partition-by str/blank? lines)]
    {:rules rules
     :messages messages}))

(def input (let [lines (str/split-lines (slurp (io/resource "d19.txt")))]
             (build-data lines)))

(def example
  (let [lines ["0: 4 1 5"
               "1: 2 3 | 3 2"
               "2: 4 4 | 5 5"
               "3: 4 5 | 5 4"
               "4: \"a\""
               "5: \"b\""
               ""
               "ababbb"
               "bababa"
               "abbbab"
               "aaabbb"
               "aaaabbb"]]
    (build-data lines)))

(defn count-valid-messages
  [{:keys [rules messages]}]
  (let [parser (insta/parser (build-grammar rules))]
    (->> messages
         (filter #(seq (insta/parses parser % :start :0)))
         count)))

(count-valid-messages example)
(count-valid-messages input)

(def extra ["8: 42 | 42 8"
            "11: 42 31 | 42 11 31"])

(count-valid-messages (update input :rules concat extra))
