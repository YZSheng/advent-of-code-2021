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

(defn get-accumulator-fn [parsed]
  (let [v (:value parsed)]
    (case (:command parsed)
      "forward" (fn [acc]
                  (-> acc
                      (update-in [:horizontal] + v)
                      (update-in [:depth] + (* v (:aim acc)))))
      "down" (fn [acc]
               (update-in acc [:aim] + v))
      "up" (fn [acc]
             (update-in acc [:aim] - v)))))

(defn accumulator [acc current]
  (let [func (get-accumulator-fn current)]
    (func acc)))

(def original-state {:horizontal 0 :depth 0 :aim 0})

(defn part-two-solution [input]
  (->> input
       (map parse-input)
       (reduce accumulator original-state)
       (#(* (:horizontal %) (:depth %)))))

(part-two-solution part-one-input)
