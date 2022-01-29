(ns day13.solution
  (:require [clojure.string :as string]))

(def sample-input "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

(def part-one-input (slurp "resources/day13/part_one.txt"))

sample-input
part-one-input

(string/split-lines sample-input)

(defn parse-dots [dots]
  (->> dots
       (map #(string/split % #","))
       (map (fn [nums]
              (into [] (map #(Integer/parseInt %) nums))))))

(defn parse-instruction [instruction]
  (-> instruction
      (string/split #"\s")
      last
      (string/split #"=")
      ((fn [[axis num]]
         [axis (Integer/parseInt num)]))))

(defn parse-input [input]
  (let [[instructions dots] ((juxt filter remove)
                             #(string/starts-with? % "f")
                             (remove empty? (string/split-lines input)))
        parsed-instructions (map parse-instruction instructions)
        parsed-dots (parse-dots dots)]
    [parsed-instructions parsed-dots]))

(parse-input sample-input)

(defn fold-horizontally [[x y] fold-at]
  (if (<= y fold-at)
    [x y]
    [x (- (* 2 fold-at) y)]))

(defn fold-vertically [[x y] fold-at]
  (if (<= x fold-at)
    [x y]
    [(- (* 2 fold-at) x) y]))

(defn fold [point direction fold-at]
  (if (= "x" direction)
    (fold-vertically point fold-at)
    (fold-horizontally point fold-at)))

(defn part-one-solution [input]
  (let [[instructions dots] (parse-input input)
        [direction fold-at] (first instructions)]
    (->> dots
         (map #(fold % direction fold-at))
         distinct
         count)))

(part-one-solution sample-input)
(part-one-solution part-one-input)

(comment
  (fold-horizontally '(5 5) 3)
  (fold-vertically '(5 5) 4)

  (-> "fold along x=655"
      (string/split #"\s")
      last
      (string/split #"=")
      ((fn [[axis num]]
         [axis (Integer/parseInt num)])))

  (->> ["6,10" "0,14"]
       (map #(string/split % #","))
       (map (fn [nums]
              (map #(Integer/parseInt %) nums))))

  ((juxt filter remove) #(string/starts-with? % "f")
                        (remove empty? (string/split-lines sample-input))))