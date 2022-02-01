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

(defn get-unvisited-neighbours [x y m visited]
  (let [n (get-all-neighbours x y m)]
    (remove (fn [neighbour]
              (some #{neighbour} visited)) n)))

(defn cost-map [m]
  (into {} (for [x (range (count (first m)))
                 y (range (count m))]
             [[x y] {:cost (if (and (zero? x) (zero? y)) 0 Integer/MAX_VALUE)
                     :from (if (and (zero? x) (zero? y)) [0 0] nil)}])))

(cost-map parsed-map)

(defn update-cost-map-by-point [cm p neighbour parsed-map]
  (let [current-cost (:cost (cm p))
        neighbour-cost (:cost (cm neighbour))
        neighbour-from (:from (cm neighbour))
        neighbour-value (get-points-in-map
                         (first neighbour)
                         (last neighbour)
                         parsed-map)
        updated-neighbour-cost (min neighbour-cost (+ current-cost neighbour-value))
        is-updated (> neighbour-cost (+ current-cost neighbour-value))
        cm (assoc cm neighbour {:cost updated-neighbour-cost
                                :from (if-not is-updated
                                        neighbour-from
                                        p)})]
    (if is-updated
      (update-cost-map cm neighbour (get-all-neighbours (first neighbour) (last neighbour) parsed-map) parsed-map)
      cm)))

(defn update-cost-map [cm p neighbours parsed-map]
  (if (empty? neighbours)
    cm
    (let [updated-cm (update-cost-map-by-point cm p (first neighbours) parsed-map)]
      (recur updated-cm p (rest neighbours) parsed-map))))

(defn get-all-points [m]
  (for [x (range (count (first m)))
        y (range (count m))]
    [x y]))

(defn construct-cost-map [m]
  (loop [visited #{}
         unvisited (get-all-points m)
         cm (cost-map m)]
    (if (empty? unvisited) cm
        (let [p (first unvisited)
              visited-updated (conj visited p)
              p-neighbours (get-all-neighbours (first p) (last p) m)]
          (recur visited-updated (rest unvisited) (update-cost-map cm p p-neighbours m))))))

parsed-map

(construct-cost-map [[1 2] [1 1]])
(construct-cost-map parsed-map)

(cost-map [[1 1] [2 2]])
(construct-cost-map [[1 2 3] [4 5 6] [7 8 9]])
(construct-cost-map [[1 1 1 1] [9 9 9 1] [9 1 1 1]])

(get-unvisited-neighbours 0 0 [[1 1] [2 2]] #{[0 0]})
(get-unvisited-neighbours (first [1 0]) (last [1 0]) [[1 1] [2 2]] (conj #{[0 0]} [1 0]))

(get (construct-cost-map parsed-map) [9 9])

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

(expand-map-right [[1 1] [2 2]] 2)

(defn expand-down [m]
  (let [addition (mapv (fn [row] (mapv inc-val row)) m)]
    addition))

(expand-down [[1 1] [2 2]])

(defn expand-map-down [m n]
  (concat m (apply concat (take n (rest (iterate expand-down m))))))


(expand-map-down [[1 1] [2 2]] 2)

(defn expand-map [m n]
  (let [expanded-right (expand-map-right m n)
        expanded-down (expand-map-down expanded-right n)]
    expanded-down))

(expand-map [[1 1] [2 2]] 2)
(expand-map parsed-map (dec 5))
(get-points-in-map 0 0 (expand-map (parse-input part-one-input) (dec 5)))
(get-points-in-map 499 499 (expand-map (parse-input part-one-input) (dec 5)))

(defn part-two-solution [input]
  (let [parsed-map (expand-map (parse-input input) 4)
        x-bound (dec (count (first parsed-map)))
        y-bound (dec (count parsed-map))
        cost-with-last (:cost (get (construct-cost-map parsed-map) [x-bound y-bound]))]
    (+ (- cost-with-last (get-points-in-map 0 0 parsed-map)) (get-points-in-map x-bound y-bound parsed-map))))

(part-two-solution sample-input)
(part-two-solution part-one-input)