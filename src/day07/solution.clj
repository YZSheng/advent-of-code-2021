(ns day07.solution
  (:require [clojure.string :as string]))

(def sample-input "16,1,2,0,4,2,7,1,2,14")
(def part-one-input (->> "resources/day07/part_one.txt"
                         slurp))

(defn parse-input [input]
  (map #(Integer/parseInt %) (string/split input #",")))

(parse-input sample-input)

(defn get-distance [parsed-input n]
  (->> parsed-input
       (map #(Math/abs (- n %)))
       (reduce +)))

(let [max-input (apply max (parse-input sample-input))
      min-input (apply min (parse-input sample-input))]
  (->> (range min-input (inc max-input))
       (map #(get-distance (parse-input sample-input) %))
       (apply min)))

(defn part-one-solution [input]
  (let [parsed-input (parse-input input)
        max-input (apply max parsed-input)
        min-input (apply min parsed-input)]
    (->> (range min-input (inc max-input))
         (map #(get-distance parsed-input %))
         (apply min))))

(part-one-solution sample-input)
(part-one-solution part-one-input)