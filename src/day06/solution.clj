(ns day06.solution
  (:require [clojure.string :as string]))

(def sample-input "3,4,3,1,2")
(def part-one-input (->> "resources/day06/part_one.txt"
                         slurp))

(defn parse-input [input]
  (map #(Integer/parseInt %) (string/split input #",")))

(defn spawn [state]
  (let [eights (get state 8 0)
        sixes (get state 6 0)
        minus-ones (get state -1 0)
        state-with-six-updated (into {} (map (fn [[k v]]
                                               [(if (= -1 k) 6 k) v]) state))]
    (if (pos? minus-ones) (assoc state-with-six-updated 8 (+ eights minus-ones) 6 (+ minus-ones sixes))
        state-with-six-updated)))

(defn age [state]
  (->> state
       (map (fn [[k v]]
              [(dec k) v]))
       (into {})
       (spawn)))

(defn part-two-solution [input n]
  (loop [state (frequencies (parse-input input))
         days n]
    (if (= 0 days)
      (reduce + (vals state))
      (recur (age state) (dec days)))))

(part-two-solution sample-input 80)
(part-two-solution part-one-input 80)

(part-two-solution sample-input 256)
(part-two-solution part-one-input 256)
