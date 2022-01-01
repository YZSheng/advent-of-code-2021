(ns day07.solution
  (:require [clojure.string :as string]))

(def sample-input "16,1,2,0,4,2,7,1,2,14")
(def part-one-input (->> "resources/day07/part_one.txt"
                         slurp))

(defn parse-input [input]
  (map #(Integer/parseInt %) (string/split input #",")))

(defn get-distance [parsed-input n]
  (->> parsed-input
       (map #(Math/abs (- n %)))
       (reduce +)))

(defn get-distance-stepped [parsed-input n]
  (->> parsed-input
       (map #(reduce + (range (inc (Math/abs (- n %))))))
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

(defn part-two-solution [input]
  (let [parsed-input (parse-input input)
        max-input (apply max parsed-input)
        min-input (apply min parsed-input)]
    (->> (range min-input (inc max-input))
         (map #(get-distance-stepped parsed-input %))
         (apply min))))

(comment
  (parse-input sample-input)
  (get-distance-stepped (parse-input sample-input) 2))

(part-one-solution sample-input)
(part-one-solution part-one-input)

(part-two-solution sample-input)
(part-two-solution part-one-input) ;; quite slow, may need to convert input into frequencies and only calculate for distinct values