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

(defn get-distance-freq [parsed-input n]
  (->> parsed-input
       frequencies
       (map (fn [[k v]]
              (let [uniq-fuel (reduce + (range (inc (Math/abs (- n k)))))]
                (* v uniq-fuel))))
       (reduce +)))

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
         (map #(get-distance-freq parsed-input %))
         (apply min))))

(comment
  (parse-input sample-input)
  (get-distance-stepped (parse-input sample-input) 2)
  (def parsed-input (parse-input sample-input))
  parsed-input
  (frequencies parsed-input)
  (get-distance-freq parsed-input 5))

(part-one-solution sample-input)
(part-one-solution part-one-input)

(part-two-solution sample-input)
(part-two-solution part-one-input)