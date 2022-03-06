(ns day17.solution)

(defn get-next-speed-x [s-x]
  (cond
    (neg? s-x) (inc s-x)
    (pos? s-x) (dec s-x)
    :else 0))

(defn get-next-position [[curr-x curr-y] [s-x s-y]]
  [(+ curr-x s-x) (+ curr-y s-y)])

(defn update-speed [[s-x s-y] n]
  (if (zero? n)
    [s-x s-y]
    (recur [(get-next-speed-x s-x) (dec s-y)] (dec n))))

(defn get-nth-position [[s-x s-y] n]
  (loop [next-pos [0 0]
         n n]
    (if (zero? n)
      next-pos
      (recur (get-next-position next-pos (update-speed [s-x s-y] (dec n))) (dec n)))))

(defn is-in-range [[x y] [min-x max-x min-y max-y]]
  (and (<= min-x x max-x) (<= min-y y max-y)))

(defn will-be-in-range [[s-x s-y] [min-x max-x min-y max-y]]
  (loop [step 0]
    (let [[x y] (get-nth-position [s-x s-y] step)]
      (cond
        (is-in-range [x y] [min-x max-x min-y max-y]) true
        (< y min-y) false
        :else (recur (inc step))))))

(defn get-total-x-distance [s-x]
  (->> s-x
       inc
       range
       (apply +)))

(defn get-total-x [s-x]
  (if (neg? s-x)
    (* -1 (get-total-x-distance (* -1 s-x)))
    (get-total-x-distance s-x)))

(defn get-speed-x-range [min-x max-x]
  (loop [xs []
         s-x 0]
    (let [total-x (get-total-x s-x)]
      (if (> total-x max-x)
        xs
        (recur (if (< total-x min-x) xs (conj xs s-x)) (inc s-x))))))

(defn get-peak-y [y]
  (apply + (range (inc y))))

(defn part-one-solution [min-x max-x min-y max-y]
  (get-peak-y (dec (Math/abs min-y))))

(part-one-solution 20 30 -10 -5)
(part-one-solution 94 151 -156 -103)

(comment
  (get-next-position [0 0] [7 2])
  (get-next-position [7 2] [7 2])
  (get-nth-position [7 2] 0)
  (get-nth-position [7 2] 1)
  (get-nth-position [7 2] 2)
  (get-nth-position [7 2] 3)
  (get-nth-position [7 2] 4)
  (get-nth-position [7 2] 5)
  (get-nth-position [7 2] 6)
  (get-nth-position [7 2] 7)
  (get-nth-position [7 2] 8)
  (is-in-range (get-nth-position [7 2] 7) [20 30 -10 -5])
  (will-be-in-range [7 2] [20 30 -10 -5])
  (will-be-in-range [6 0] [20 30 -10 -5])
  (will-be-in-range [6 3] [20 30 -10 -5])
  (will-be-in-range [9 0] [20 30 -10 -5])
  (will-be-in-range [17 -4] [20 30 -10 -5])
  (get-total-x 6)
  (get-total-x -6)


  (get-speed-x-range 20 30))






