(ns day02.solution)

(def sample-input ["forward 5"
                   "down 5"
                   "forward 8"
                   "up 3"
                   "down 8"
                   "forward 2"])

(defn parse-input [input]
  (let [[command v] (clojure.string/split input #"\s")]
    {:command command :value (Integer/parseInt v)}))

(parse-input "forward 5")

(defn transform-result [input]
  (into {}
        (map (fn [[k v]]
               [(keyword k) (reduce + (map :value v))])
             (group-by :command (map parse-input input)))))

(defn calculate-result [parsed-result]
  (let [{:keys [forward down up]} parsed-result]
    (* (Math/abs (- down up)) forward)))

(def part-one-input (->> "resources/day02/part_one.txt"
                         slurp
                         clojure.string/split-lines))
(defn part-one-solution [input]
  (->> input
       transform-result
       calculate-result))

(part-one-solution part-one-input)