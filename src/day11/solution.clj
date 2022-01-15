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

(defn parse-input [input]
  (map (fn [line]
         (map #(Integer/parseInt %) (string/split line #""))) (string/split-lines input)))

(defn increase-all-by-one [parsed-map]
  (map #(map inc %) parsed-map))

;; for each step, track flashed points
;; if the given map does not contain new points to flash, return [map count]
;; else, get the new point to flash, update neighbours to get the map, recur

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
    (> 9 v) 0
    :else (inc v)))

(defn increase-neighbours [neighbours parsed-map]
  (if (empty? neighbours) parsed-map
      (let [f (first neighbours)
            r (rest neighbours)
            fx (:x f)
            fy (:y f)
            fv (get-points-in-map fx fy parsed-map)
            updated-map (update-in-map fx fy (get-next-value fv) parsed-map)]
        (increase-neighbours r updated-map))))

(defn flash-one-point [p m]
  (let [{:keys [x y]} p
        neighbours (get-surrounding-points x y m)]
    (increase-neighbours neighbours m)))

(comment
  (def test-map (parse-input test-input))
  test-map
  (def after-increase-step-1 (increase-all-by-one test-map))
  after-increase-step-1
  (get-points-to-flash after-increase-step-1))