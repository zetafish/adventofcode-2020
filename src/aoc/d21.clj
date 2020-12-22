(ns aoc.d21
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [instaparse.core :as insta]))

(def grammar
  "
  <E>=ingredients <' (contains '> alergens <')'>
  ingredients=word (<' '> word)*
  alergens=word (<', '> word)*
  word=#'\\w+'
  ")

(defn parse-food [s]
  (zipmap [:ingredients :alergens]
          (insta/transform
           {:ingredients vector
            :alergens vector
            :word keyword}
           (insta/parse
            (insta/parser grammar) s))))

(def input (map parse-food (str/split-lines (slurp (io/resource "d21.txt")))))

(def example (map parse-food ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
                              "trh fvjkl sbzzf mxmxvkd (contains dairy)"
                              "sqjhc fvjkl (contains soy)"
                              "sqjhc mxmxvkd sbzzf (contains fish)"]))

(defn update-vals
  [m f & args]
  (into {} (map (fn [[k v]] [k (apply f v args)]) m)))

(defn potential-alergic-ingredients
  [foods alergen]
  (->> foods
       (filter #(contains? (set (:alergens %)) alergen))
       (map (comp set :ingredients))
       (apply set/intersection)))

(defn build-alergen-map
  [foods]
  (let [alergens (set (mapcat :alergens foods))
        ingredients (map #(potential-alergic-ingredients foods %) alergens)]
    (zipmap alergens ingredients)))

(defn blacklist [alergen-map]
  (loop [result {} alergen-map alergen-map]
    (if (empty? alergen-map)
      (update-vals result first)
      (let [items (filter (fn [[_ v]] (= 1 (count v))) alergen-map)
            alergens (map first items)
            ingredients (mapcat second items)]
        (recur (into result items)
               (-> alergen-map
                   (#(apply dissoc % alergens))
                   (update-vals #(apply disj % ingredients))))))))

(defn safe-foods [foods blacklist]
  (let [ingredients (mapcat :ingredients foods)
        dangerous (set (vals blacklist))
        safe (remove dangerous ingredients)]
    {:safe safe
     :amount (count safe)}))

(defn analyze-foods [foods]
  (safe-foods foods
              (blacklist (build-alergen-map foods))))

;; part 1
(analyze-foods example)
(analyze-foods input)

(defn canonical-blacklist
  [foods]
  (->> foods
       build-alergen-map
       blacklist
       (sort-by first)
       (map (comp name second))
       (str/join ",")))

;; part 2
(println (canonical-blacklist example))
(println (canonical-blacklist input))
