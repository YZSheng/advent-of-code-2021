(ns advent-of-code-2021.day01)

(def sample-input [199
                   200
                   208
                   210
                   200
                   207
                   240
                   269
                   260
                   263])

(def part-one-input (->> "resources/day01/part_one.txt"
                         slurp
                         clojure.string/split-lines
                         (map #(Integer/parseInt %))))

(defn part-one-solution [input]
  (->> input
       (partition 2 1)
       (map #(if (> (second %) (first %)) 1 0))
       (reduce +)))

(part-one-solution part-one-input)

(defn part-two-solution [input]
  (->> input
       (partition 3 1)
       (map #(apply + %))
       part-one-solution))

(part-two-solution part-one-input)

(comment
  part-one-input
  (part-one-solution sample-input)
  (part-two-solution sample-input)

  (->> sample-input
       (map inc)
       (reduce +))
  (part-one-solution (map #(apply + %)
                          (partition 3
                                     (interleave sample-input
                                                 (drop 1 sample-input)
                                                 (drop 2 sample-input))))))
