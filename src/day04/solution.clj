(ns day04.solution
  (:require [clojure.string :as string]))

(def sample-input "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

(def part-one-input (->> "resources/day04/part_one.txt"
                         slurp))

(defn cleanup [input]
  (map #(Integer/parseInt %)
       (-> input
           string/trim
           (string/split #",|\s+"))))

(defn get-parsed-input [input]
  (map cleanup (filter (comp not empty?) (string/split input #"\n"))))

(defn get-val-in-map [m x y]
  (-> m
      (nth y)
      (nth x)))

(defn find-coord-in-map [map v]
  (first (filter (comp not nil?) (for [x (range 5)
                                       y (range 5)]
                                   (when (= (get-val-in-map map x y) v) {:x x :y y})))))

(defn win-in-direction? [coords direction]
  (some #(= (last %) 5) (map (fn [[k v]]
                               [k (count (filter identity v))]) (group-by direction coords))))

(defn win? [coords]
  (->> [:x :y]
       (map #(win-in-direction? coords %))
       (some true?)))

(defn transpose [& xs]
  (apply map list xs))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn find-coord-in-all-maps [v bingo-maps]
  (map #(find-coord-in-map % v) bingo-maps))

(defn has-winner? [draw-sequence bingo-maps]
  (->> draw-sequence
       (map #(find-coord-in-all-maps % bingo-maps))
       (apply transpose)
       (map win?)
       (map-indexed vector)
       (filter second)
       first
       first))

(defn find-winner? [n draw-sequence bingo-maps]
  (let [win (has-winner? (take n draw-sequence) bingo-maps)]
    (if win {:num (nth draw-sequence (dec n)) :map-index win :n n}
        (find-winner? (inc n) draw-sequence bingo-maps))))

(defn find-sum-of-unmarked [draw-sequence bingo-maps result]
  (apply + (filter #(not (in? (take (:n result) draw-sequence) %)) (flatten (nth bingo-maps (:map-index result))))))

(defn find-part-one-solution [input]
  (let [parsed-input (get-parsed-input input)
        draw-sequence (first parsed-input)
        bingo-maps (partition 5 (drop 1 parsed-input))
        result (find-winner? 1 draw-sequence bingo-maps)]
    (* (find-sum-of-unmarked draw-sequence bingo-maps result) (:num result))))

(find-part-one-solution sample-input)
(find-part-one-solution part-one-input)

(defn get-every-map-result [draw-sequence bingo-maps]
  (->> draw-sequence
       (map #(find-coord-in-all-maps % bingo-maps))
       (apply transpose)
       (map win?)
       (map-indexed vector)))

(comment
  (def parsed-input (get-parsed-input sample-input))
  (def draw-sequence (first parsed-input))
  draw-sequence
  (def bingo-maps (partition 5 (drop 1 parsed-input)))
  (get-every-map-result [7 4 9 5 11 17 23 2 0 14 21 24 10 16] bingo-maps)
  (first (first (filter (comp nil? second) (get-every-map-result [7 4 9 5 11 17 23 2 0 14 21 24 10 16] bingo-maps)))))

(defn get-last-unwon-map-index [result]
  (->> result
       (filter (comp nil? second))
       first
       first))

(defn all-won? [result]
  (every? #(true? (second %)) result))

(defn find-all-winner? [n draw-sequence bingo-maps index]
  (let [result (get-every-map-result (take n draw-sequence) bingo-maps)]
    (if (all-won? result) {:num (nth draw-sequence (dec n)) :map-index index :n n}
        (find-all-winner? (inc n) draw-sequence bingo-maps (get-last-unwon-map-index result)))))

(comment
  (all-won? '([0 true] [1 nil] [2 true]))
  (all-won? '([0 true] [1 true] [2 true]))
  (get-last-unwon-map-index '([0 true] [1 nil] [2 true]))
  (find-all-winner? 1 draw-sequence bingo-maps -1))

(defn find-part-two-solution [input]
  (let [parsed-input (get-parsed-input input)
        draw-sequence (first parsed-input)
        bingo-maps (partition 5 (drop 1 parsed-input))
        result (find-all-winner? 1 draw-sequence bingo-maps -1)]
    (* (find-sum-of-unmarked draw-sequence bingo-maps result) (:num result))))

(find-part-two-solution sample-input)
(find-part-two-solution part-one-input)