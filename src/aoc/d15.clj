(ns aoc.d15)

(def input [1 0 15 2 10 13])

(defn say [state n]
  (-> state
      (assoc :last n)
      (update :turn (fnil inc 0))
      (update-in [:history n] (fnil conj nil) (:turn state))
      (update :seq (fnil conj []) n)))


(defn what-to-say
  [{:keys [last turns history]}]
  (if (= 1 (count (history last)))
    0
    (apply - (take 2 (history last)))))

(defn say-what-must-be-said
  [state]
  (say state (what-to-say state)))

(defn init [col]
  (reduce say {:turn 0} col))

(-> (init [0 3 6])
    say-what-must-be-said
    say-what-must-be-said
    say-what-must-be-said
    say-what-must-be-said
    say-what-must-be-said
    say-what-must-be-said
    say-what-must-be-said
    )

(->> (init [0 3 6])
     (iterate say-what-must-be-said)
     (drop-while #(not= 2020 (:turn %)))
     first :last)

(->> (init input)
     (iterate say-what-must-be-said)
     (drop-while #(not= 2020 (:turn %)))
     first :last)

(->> (init input)
     (iterate say-what-must-be-said)
     (drop (- 30000000 (count input)))
     first :last)
