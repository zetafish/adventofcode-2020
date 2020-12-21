(ns aoc.d21
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [instaparse.core :as insta]
            [clojure.set :as set]))

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

(defn disj-from-vals
  [m coll]
  (update-vals m #(apply disj % coll)))

(disj-from-vals {:a #{1 2 3 4}
                 :b #{1 2 3 5 6 7}}
                [1 2])

;; (defn alergy-options
;;   [{:keys [ingredients alergens]}]
;;   (let [n (count alergens)]
;;     (map zipmap
;;          (repeat alergens)
;;          (mapcat combo/permutations (combo/combinations (list ingredients) n)))))

;; (defn conflict?
;;   [m1 m2]
;;   (let [ks (set/intersection (set (keys m1)) (set (keys m2)))]
;;     (not= (select-keys m1 ks)
;;           (select-keys m2 ks))))

;; (defn options-conflict?
;;   "Check if `m` conflict with all possible options"
;;   [options m]
;;   (every? #(conflict? % m) options))

;; (defn options-list-conflict?
;;   [options-list m]
;;   (some #(options-conflict? % m) options-list))

;; (defn analyze [foods]
;;   (let [state (atom [])]
;;     (letfn [(f [blacklist options-list]
;;               (if (empty? options-list)
;;                 (swap! state conj blacklist)
;;                 (let [mm (->> (first options-list)
;;                               (remove #(conflict? blacklist %))
;;                               (remove #(options-list-conflict? (rest options-list) %)))]
;;                   (doseq [m mm]
;;                     (println "trying" m "on" blacklist)
;;                     (f (merge blacklist m) (rest options-list))))))]
;;       (f {} (map alergy-options foods))
;;       @state)))



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
