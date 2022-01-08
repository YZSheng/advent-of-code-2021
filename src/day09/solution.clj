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
       string/split-lines
       (map #(string/split % #""))
       (map (fn [nums] (map #(Integer/parseInt %) nums)))))

(defn get-in-height-map [x y m]
  (-> m
      (nth y)
      (nth x)))

(defn get-north-neighbour [x y m]
  (if (>= 0 y) 9
      (get-in-height-map x (dec y) m)))

(defn get-south-neighbour [x y m]
  (if (<= (count m) (inc y)) 9
      (get-in-height-map x (inc y) m)))

(defn get-east-neighbour [x y m]
  (if (<= (count (first m)) (inc x)) 9
      (get-in-height-map (inc x) y m)))

(defn get-west-neighbour [x y m]
  (if (>= 0 x) 9
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

(defn get-neighbour-points [current-point m]
  (let [x-bound (count (first m))
        y-bound (count m)
        {:keys [x y v]} current-point
        [east south west north] (get-neighbours x y m)
        east-point (if (>= (inc x) x-bound) nil {:x (inc x) :y y :v east})
        south-point (if (>= (inc y) y-bound) nil {:x x :y (inc y) :v south})
        west-point (if (>= 0 x) nil {:x (dec x) :y y :v west})
        north-point (if (>= 0 y) nil {:x x :y (dec y) :v north})]
    (->> [east-point south-point west-point north-point]
         (filter identity)
         (filter #(> 9 (:v %) v)))))

(defn update-results [current-point next-points results m]
  (let [neighbours (get-neighbour-points current-point m)
        updated-next-points (set (concat neighbours next-points))
        updated-results (clojure.set/union results #{current-point})]
    [updated-next-points updated-results]))

(defn find-basin-with-results [current-point next-points results m]
  (let [[updated-next-points updated-results] (update-results current-point next-points results m)]
    (if (empty? updated-next-points)
      updated-results
      (find-basin-with-results (first updated-next-points) (set (rest updated-next-points)) updated-results m))))

(defn get-basin-size [p m]
  (count (find-basin-with-results p
                                  (set (get-neighbour-points p m))
                                  #{p}
                                  m)))

(defn find-all-lowest-points-with-coord [m]
  (for [x (range (count (first m)))
        y (range (count m))
        :when (is-lowest? x y m)]
    {:x x :y y :v (get-in-height-map x y m)}))

(defn part-two-solution [input]
  (let [m (parse-input input)]
    (->> m
         find-all-lowest-points-with-coord
         (map #(get-basin-size % m))
         sort
         reverse
         (take 3)
         (apply *))))

(part-two-solution sample-input)
(part-two-solution part-one-input)