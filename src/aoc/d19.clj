(ns aoc.d19
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [instaparse.core :as insta]))

;; (def rule "
;;   <E>=num <': '> (a|b|and|or)
;;   num=#'\\d+'
;;   a=#'\"a\"'
;;   b=#'\"b\"'
;;   and=num (<' '> num)*
;;   or=and <' | '> and")

;; (defn parse-rule [s]
;;   (vec
;;    (insta/transform
;;     {:num #(Integer/parseInt %)
;;      :a (constantly \a)
;;      :b (constantly \b)}
;;     (insta/parse
;;      (insta/parser rule) s))))

(defn build-grammar [rules]
  (->> rules
       (map #(str/replace % #":" "="))
       (str/join "\n")))

(defn build-data [lines]
  (let [[rules _ messages] (partition-by str/blank? lines)]
    {:grammar (build-grammar rules)
     :parser (insta/parser (build-grammar rules))
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
  [{:keys [parser messages]}]
  (->> messages
       (filter #(seq (insta/parses parser % :start :0)))
       count))

(count-valid-messages example)
(count-valid-messages input)
