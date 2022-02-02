(ns day15.solution
  (:require [clojure.string :as string]))

(def sample-input "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")

(def part-one-input (slurp "resources/day15/part_one.txt"))

(defn parse-input [input]
  (map (fn [line]
         (map #(Integer/parseInt %) (string/split line #""))) (string/split-lines input)))

(parse-input sample-input)

(def parsed-map (parse-input sample-input))

(defn get-points-in-map [x y parsed-map]
  (nth (nth parsed-map y []) x -1))

(defn get-neighbour-up [x y]
  (if (= 0 y) nil
      [x (dec y)]))

(defn get-neighbour-down [x y m]
  (if (= y (dec (count m))) nil
      [x (inc y)]))

(defn get-neighbour-left [x y]
  (if (zero? x) nil
      [(dec x) y]))

(defn get-neighbour-right [x y m]
  (if (= x (dec (count (first m)))) nil
      [(inc x) y]))

(defn get-all-neighbours [x y m]
  (->> [(get-neighbour-up x y)
        (get-neighbour-right x y m)
        (get-neighbour-down x y m)
        (get-neighbour-left x y)]
       (filter identity)))

(defn cost-map [m]
  (into {} (for [x (range (count (first m)))
                 y (range (count m))]
             [[x y] {:cost (if (and (zero? x) (zero? y)) 0 Integer/MAX_VALUE)}])))

(defn update-cost-map-by-point [cm p neighbour parsed-map]
  (let [current-cost (:cost (cm p))
        neighbour-cost (:cost (cm neighbour))
        neighbour-value (get-points-in-map
                         (first neighbour)
                         (last neighbour)
                         parsed-map)
        updated-neighbour-cost (min neighbour-cost (+ current-cost neighbour-value))
        is-updated (> neighbour-cost (+ current-cost neighbour-value))
        cm (assoc cm neighbour {:cost updated-neighbour-cost})]
    [is-updated cm]))

(defn update-cost-map [cm p neighbours parsed-map neighbours-to-revisit]
  (if (empty? neighbours)
    [cm neighbours-to-revisit]
    (let [[is-updated updated-cm] (update-cost-map-by-point cm p (first neighbours) parsed-map)]
      (recur updated-cm p (rest neighbours) parsed-map (if is-updated
                                                         (conj neighbours-to-revisit (first neighbours))
                                                         neighbours-to-revisit)))))

(defn construct-cost-map [m]
  (loop [unvisited (conj clojure.lang.PersistentQueue/EMPTY [0 0])
         cm (cost-map m)]
    (if (empty? unvisited) cm
        (let [p (peek unvisited)
              p-neighbours (get-all-neighbours (first p) (last p) m)
              [updated-cm neighbours-to-revisit] (update-cost-map cm p p-neighbours m [])]
          (recur (into (pop unvisited) neighbours-to-revisit) updated-cm)))))

(defn part-one-solution [input]
  (let [parsed-map (parse-input input)
        x-bound (dec (count (first parsed-map)))
        y-bound (dec (count parsed-map))]
    (:cost (get (construct-cost-map parsed-map) [x-bound y-bound]))))

(part-one-solution sample-input)
(part-one-solution part-one-input)

(defn- inc-val [val]
  (if (= 9 val)
    1
    (inc val)))

(defn expand-right [row]
  (mapv inc-val row))

(expand-right [1 1])
(take 2 (rest (iterate expand-right [1 1])))

(defn expand-map-right [m n]
  (map (fn [row]
         (flatten (concat (list row) (take n (rest (iterate expand-right row)))))) m))

(defn expand-down [m]
  (let [addition (mapv (fn [row] (mapv inc-val row)) m)]
    addition))

(defn expand-map-down [m n]
  (concat m (apply concat (take n (rest (iterate expand-down m))))))

(defn expand-map [m n]
  (let [expanded-right (expand-map-right m n)
        expanded-down (expand-map-down expanded-right n)]
    expanded-down))

(defn part-two-solution [input]
  (let [parsed-map (expand-map (parse-input input) 4)
        x-bound (dec (count (first parsed-map)))
        y-bound (dec (count parsed-map))]
    (:cost (get (construct-cost-map parsed-map) [x-bound y-bound]))))

(part-two-solution sample-input)
(part-two-solution part-one-input)