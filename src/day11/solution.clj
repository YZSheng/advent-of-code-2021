(ns day11.solution
  (:require [clojure.string :as string]))

(def sample-input "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(def test-input "11111
19991
19191
19991
11111")

(def part-one-input (slurp "resources/day11/part_one.txt"))

part-one-input

(defn parse-input [input]
  (map (fn [line]
         (map #(Integer/parseInt %) (string/split line #""))) (string/split-lines input)))

(defn increase-all-by-one [parsed-map]
  (map #(map inc %) parsed-map))

;; for each step, track flashed points
;; if the given map does not contain new points to flash, return [map count]
;; else, get the new point to flash, update neighbours to get the map, recur with increased count

(defn get-points-in-map [x y parsed-map]
  (nth (nth parsed-map y []) x -1))

(defn has-flashed? [flashed p]
  (some #(and (= (:x %) (:x p)) (= (:y %) (:y p))) flashed))

(defn get-points-to-flash [parsed-map]
  (for [x (range (count (first parsed-map)))
        y (range (count parsed-map))
        :let [v (get-points-in-map x y parsed-map)]
        :when (> v 9)]
    {:x x :y y}))

(defn map-contains-new-flash-point? [m existing-points]
  (let [points (get-points-to-flash m)]
    (some #(not (has-flashed? existing-points %)) points)))

(defn get-surrounding-points [x y parsed-map]
  (for [updated-x [(dec x) x (inc x)]
        updated-y [(dec y) y (inc y)]
        :when (and (> (get-points-in-map updated-x updated-y parsed-map) 0)
                   (or (not= x updated-x) (not= y updated-y)))]
    {:x updated-x :y updated-y}))

(defn update-in-map [x y v parsed-map]
  (assoc (into [] parsed-map) y (assoc (into [] (nth parsed-map y)) x v)))

(defn count-zeros [m]
  (count (filter zero? (flatten m))))

(defn get-next-value [v]
  (cond
    (= 0 v) 0
    (< 9 v) 0
    :else (inc v)))

(get-next-value 2)

(defn increase-neighbours [neighbours parsed-map]
  (if (empty? neighbours) parsed-map
      (let [f (first neighbours)
            r (rest neighbours)
            fx (:x f)
            fy (:y f)
            fv (get-points-in-map fx fy parsed-map)
            updated-map (update-in-map fx fy (inc fv) parsed-map)]
        (increase-neighbours r updated-map))))

(defn flash-one-point [p m]
  (let [{:keys [x y]} p
        neighbours (get-surrounding-points x y m)]
    (increase-neighbours neighbours (update-in-map x y 0 m))))

(defn perform-one-step [parsed-map]
  (loop [m (increase-all-by-one parsed-map)
         points-to-flash (get-points-to-flash m)
         flashed-points []]
    (if (and (not-empty points-to-flash)
             (map-contains-new-flash-point? m flashed-points))
      (let [updated-map (flash-one-point (first points-to-flash) m)
            new-points-to-flash (get-points-to-flash updated-map)]
        (recur (flash-one-point (first points-to-flash) m) (dedupe (concat new-points-to-flash (rest points-to-flash))) (conj flashed-points (first points-to-flash))))
      [m flashed-points])))

(defn perform-n-steps [parsed-map n]
  (loop [c 0
         m parsed-map
         n n]
    (if (zero? n)
      c
      (let [[updated-map flashed-points] (perform-one-step m)]
        (recur (+ c (count flashed-points)) updated-map (dec n))))))

(defn part-one-solution [input]
  (perform-n-steps (parse-input input) 100))

(part-one-solution sample-input)
(part-one-solution part-one-input)

(comment
  (def test-map (parse-input test-input))
  test-map
  (def after-increase-step-1 (increase-all-by-one test-map))
  after-increase-step-1
  (get-points-to-flash after-increase-step-1)

  test-map
  (map-contains-new-flash-point? after-increase-step-1 [])

  (flash-one-point {:x 1 :y 1} after-increase-step-1)


  (loop [m (increase-all-by-one test-map)
         points-to-flash (get-points-to-flash m)
         flashed-points []]
    (if (and (not-empty points-to-flash)
             (map-contains-new-flash-point? m flashed-points))
      (let [updated-map (flash-one-point (first points-to-flash) m)
            new-points-to-flash (get-points-to-flash updated-map)]
        (recur (flash-one-point (first points-to-flash) m) (dedupe (concat new-points-to-flash (rest points-to-flash))) (conj flashed-points (first points-to-flash))))
      [m flashed-points]))

  (perform-one-step test-map)

  (get-points-to-flash [[3 4 5 4 3] [4 0 0 0 4] [5 0 10 0 5] [4 0 0 0 4] [3 4 5 4 3]])

  (perform-n-steps test-map 2)
  (perform-n-steps (parse-input sample-input) 100))