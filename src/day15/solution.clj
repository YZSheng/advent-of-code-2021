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

(defn get-neighbour-up [x y m]
  (if (= 0 y) nil
      {:x x
       :y (dec y)
       :v (get-points-in-map x (dec y) m)}))

(get-neighbour-up 0 1 parsed-map)

(defn get-neighbour-down [x y m]
  (if (= y (dec (count m))) nil
      {:x x
       :y (inc y)
       :v (get-points-in-map x (inc y) m)}))

(get-neighbour-down 9 8 parsed-map)

(defn get-neighbour-left [x y m]
  (if (zero? x) nil
      {:x (dec x)
       :y y
       :v (get-points-in-map (dec x) y m)}))

(get-neighbour-left 1 0 parsed-map)

(defn get-neighbour-right [x y m]
  (if (= x (dec (count (first m)))) nil
      {:x (inc x)
       :y y
       :v (get-points-in-map (inc x) y m)}))

(get-neighbour-right 8 9 parsed-map)

(defn get-all-neighbours [x y m cost]
  (->> [(get-neighbour-up x y m)
        (get-neighbour-right x y m)
        (get-neighbour-down x y m)
        (get-neighbour-left x y m)]
       (filter identity)
       (map (fn [p]
              (assoc p :cost (+ cost (:v p)))))))

(defn get-unvisited-neighbours [x y m visited cost]
  (let [n (get-all-neighbours x y m cost)]
    (remove (fn [neighbour]
              (some (fn [visited-p]
                      (and (= (:x visited-p) (:x neighbour))
                           (= (:y visited-p) (:y neighbour)))) visited)) n)))

(defn add-or-update [unvisited p]
  (if-let [point (first (filter #(and (= (:x p) (:x %))
                                      (= (:y p) (:y %))) unvisited))]
    (let [rest (remove #(and (= (:x p) (:x %))
                             (= (:y p) (:y %))) unvisited)]
      (conj rest (assoc point :cost (min (:cost p) (:cost point)))))
    (conj unvisited p)))

(defn add-to-unvisited [unvisited new-ps]
  (if (empty? new-ps)
    unvisited
    (recur (add-or-update unvisited (first new-ps)) (rest new-ps))))

(add-to-unvisited [{:x 1 :y 1 :v 1 :cost 2}] [{:x 1 :y 1 :v 1 :cost 2}
                                              {:x 1 :y 1 :v 1 :cost 1}
                                              {:x 1 :y 2 :v 1 :cost 1}])

(defn part-one-solution [n x y m visited unvisited cost]
  (let [neighbours (get-unvisited-neighbours x y m visited cost)
        updated-unvisited (sort-by :cost (add-to-unvisited unvisited neighbours))
        updated-visited (conj visited {:x x :y y :v (get-points-in-map x y m) :cost cost})]
    (if (and (= x n) (= y n))
      cost
      (let [[next-p & rest-unvisited] updated-unvisited]
        (recur n (:x next-p) (:y next-p) m updated-visited rest-unvisited (:cost next-p))))))

(part-one-solution 9 0 0 parsed-map #{} [] 0)
(part-one-solution 99 0 0 (parse-input part-one-input) #{} [] 0)


(comment
  (get-all-neighbours 9 9 parsed-map 10))