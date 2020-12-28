(ns aoc.d20-test
  (:require [aoc.d20 :as sut]
            [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]))

(defn world []
  (sut/build-world
   (sut/parse-input "d20-example.txt")))

(deftest x-test
  (is (= 1 1)))

(deftest transform-test
  (let [square ["PW" "GB"]]
    (is (= ["WP" "BG"] (map str/join (sut/flip-vertical-axis square))))
    (is (= ["GB" "PW"] (map str/join (sut/flip-horizontal-axis square))))
    (is (= ["PG" "WB"] (map str/join (sut/flip-main-diagonal square))))
    (is (= ["BW" "GP"] (map str/join (sut/flip-other-diagonal square))))))

(deftest plan-tile-test
  (let [w2 (sut/plan-tile (world) [0 0])]
    (is (= {:grid {[0 0] 1171} :used [1171]} (:plan w2)))))

(deftest plan-all-tiles-test
  (let [w2 (sut/plan-all-tiles (world))]
    (is (= (sort (keys (:tiles w2))) (sort (:used (:plan w2)))))
    (is (= {[0 0] 1171
            [0 1] 1489
            [0 2] 2971
            [1 0] 2473
            [1 1] 1427
            [1 2] 2729
            [2 0] 3079
            [2 1] 2311
            [2 2] 1951}
           (:grid (:plan w2))))))

(deftest spec-all-around-test
  (let [spec (sut/spec-all-around (world) [0 0])]
    (is (= [[:north :border] [:west :border]] spec))))
