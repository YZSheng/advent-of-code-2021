(ns day05.solution
  (:require [clojure.string :as string]))

(def sample-input (string/split-lines "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"))

(def part-one-input (->> "resources/day05/part_one.txt"
                         slurp
                         clojure.string/split-lines))

(defn parse-coord [coord-str]
  (map #(Integer/parseInt %) (string/split coord-str #",")))

(defn is-horizontal-or-vertical [line]
  (let [{:keys [start end]} line
        [start-x start-y] start
        [end-x end-y] end]
    (or (= start-x end-x) (= start-y end-y))))

(defn is-diagonal [line]
  (let [{:keys [start end]} line
        [start-x start-y] start
        [end-x end-y] end]
    (= (Math/abs (- start-x end-x)) (Math/abs (- start-y end-y)))))

(defn pad [n coll]
  (take n (concat coll (repeat (first coll)))))

(defn get-update-range-end-fn [start end]
  (if (> start end) dec inc))

(defn get-update-range-end-step [start end]
  (if (> start end) -1 1))

(defn get-coords-in-line [start end]
  (let [[start-x start-y] start
        [end-x end-y] end
        xs (range start-x ((get-update-range-end-fn start-x end-x) end-x) (get-update-range-end-step start-x end-x))
        ys (range start-y ((get-update-range-end-fn start-y end-y) end-y) (get-update-range-end-step start-y end-y))
        xs-padded (if (< (count xs) (count ys)) (pad (count ys) xs) xs)
        ys-padded (if (< (count ys) (count xs)) (pad (count xs) ys) ys)]
    (map vector xs-padded ys-padded)))

(defn get-all-coords [result]
  (apply concat result))

(defn part-one-solution [input]
  (->> input
       (map #(string/split % #" -> "))
       (map (fn [[start end]]
              {:start (parse-coord start) :end (parse-coord end)}))
       (filter is-horizontal-or-vertical)
       (map (fn [{:keys [start end]}]
              (get-coords-in-line start end)))
       (get-all-coords)
       frequencies
       (filter #(> (second %) 1))
       (count)))

sample-input
part-one-input

(part-one-solution sample-input)
(part-one-solution part-one-input)

(defn is-valid-direction [line]
  (or (is-horizontal-or-vertical line) (is-diagonal line)))

(defn part-two-solution [input]
  (->> input
       (map #(string/split % #" -> "))
       (map (fn [[start end]]
              {:start (parse-coord start) :end (parse-coord end)}))
       (filter is-valid-direction)
       (map (fn [{:keys [start end]}]
              (get-coords-in-line start end)))
       (get-all-coords)
       frequencies
       (filter #(> (second %) 1))
       (count)))

(part-two-solution sample-input)
(part-two-solution part-one-input)

(comment
  (parse-coord "0,9")

  (is-horizontal-or-vertical {:start '(0 9) :end '(9 9)})
  (is-valid-direction {:start '(0 9) :end '(9 9)})
  (is-diagonal {:start '(6 4) :end '(2 0)})
  (is-valid-direction {:start '(6 4) :end '(2 0)})
  (is-valid-direction {:start '(6 4) :end '(2 1)})

  (get-coords-in-line [5 1] [10 1])
  (get-coords-in-line [6 4] [2 0])
  (get-coords-in-line [5 5] [8 2])

  (map vector [5 6 7 8] [5 4 3 2]))