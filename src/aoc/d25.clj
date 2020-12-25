(ns aoc.d25)

(def example {:door {:public 17807724} :card {:public 5764801}})

(def input {:card {:public 19774466} :door {:public 7290641}})

(def other {:door :card :card :door})

(defn transformer [subject-number]
  (fn [value]
    (rem (* value subject-number) 20201227)))

(defn guess-loop-size [public-key]
  (->> (iterate (transformer 7) 1)
       (take-while #(not= % public-key))
       count))

(defn transform [f loop-size]
  (->> (iterate f 1)
       (drop loop-size)
       first))

(defn step-loop-size [state]
  (-> state
      (assoc-in [:door :loop-size] (guess-loop-size (get-in state [:door :public])))
      (assoc-in [:card :loop-size] (guess-loop-size (get-in state [:card :public])))))

(defn derive-loop-size [state thing]
  (assoc-in state
            [thing :loop-size]
            (guess-loop-size (get-in state [thing :public]))))

(defn derive-key [state thing]
  (let [subject-number (get-in state [(other thing) :public])
        f (transformer subject-number)
        loop-size (get-in state [thing :loop-size])]
    (assoc-in state [thing :key] (transform f loop-size))))

(defn crack [state]
  (-> state
      (derive-loop-size :card)
      (derive-loop-size :door)
      (derive-key :card)
      (derive-key :door)))

(println (get-in (crack example) [:card :key]))
(println (get-in (crack input) [:card :key] ))
