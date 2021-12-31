(ns day06.solution
  (:require [clojure.string :as string]))

(def sample-input "3,4,3,1,2")
(def part-one-input (->> "resources/day06/part_one.txt"
                         slurp))

(defn parse-input [input]
  (map #(Integer/parseInt %) (string/split input #",")))

(defn spawn-fish-if-needed [timers]
  (let [zeros (count (filter #(= -1 %) timers))
        resetted (map #(if (= -1 %) 6 %) timers)]
    (concat resetted (repeat zeros 8))))

(defn after-one-day [timers]
  (->> timers
       (map dec)
       spawn-fish-if-needed))

(defn after-n-days [input n]
  (if (= 0 n) input
      (after-n-days (after-one-day input) (dec n))))

(defn part-one-solution [input]
  (-> input
      parse-input
      (after-n-days 80)
      count))

(comment
  part-one-input
  (parse-input sample-input)
  (after-one-day (after-one-day (parse-input sample-input)))
  (after-n-days (parse-input sample-input) 18))

(part-one-solution sample-input)
(part-one-solution part-one-input)