(ns day09.solution
  (:require [clojure.string :as string]))

(def sample-input "2199943210
3987894921
9856789892
8767896789
9899965678")

(def part-one-input (slurp "resources/day09/part_one.txt"))

part-one-input

(defn parse-input [input]
  (->> input
       string/split-line
       (map #(string/split % #""))
       (map (fn [nums] (map #(Integer/parseInt %) nums)))))


(defn get-in-height-map [x y m]
  (-> m
      (nth y)
      (nth x)))

(defn get-north-neighbour [x y m]
  (if (>= 0 y) Integer/MAX_VALUE
      (get-in-height-map x (dec y) m)))

(defn get-south-neighbour [x y m]
  (if (<= (count m) (inc y)) Integer/MAX_VALUE
      (get-in-height-map x (inc y) m)))

(defn get-east-neighbour [x y m]
  (if (<= (count (first m)) (inc x)) Integer/MAX_VALUE
      (get-in-height-map (inc x) y m)))

(defn get-west-neighbour [x y m]
  (if (>= 0 x) Integer/MAX_VALUE
      (get-in-height-map (dec x) y m)))

(defn get-neighbours [x y m]
  ((juxt get-east-neighbour get-south-neighbour get-west-neighbour get-north-neighbour) x y m))

(defn is-lowest? [x y m]
  (let [neighbouts (get-neighbours x y m)
        current (get-in-height-map x y m)]
    (every? #(> % current) neighbouts)))

(defn find-all-lowest-points [m]
  (for [x (range (count (first m)))
        y (range (count m))
        :when (is-lowest? x y m)]
    (get-in-height-map x y m)))

(defn part-one-solution [input]
  (->> input
       parse-input
       find-all-lowest-points
       (map inc)
       (reduce +)))

(part-one-solution sample-input)
(part-one-solution part-one-input)
