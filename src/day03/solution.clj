(ns day03.solution
  (:require [clojure.string :as string]))

(def sample-input ["00100"
                   "11110"
                   "10110"
                   "10111"
                   "10101"
                   "01111"
                   "00111"
                   "11100"
                   "10000"
                   "11001"
                   "00010"
                   "01010"])

(defn transpose [& xs]
  (apply map list xs))

(comment
  (string/split "00100" #"")
  (apply transpose [["0" "0" "1" "0" "0"] ["1" "1" "1" "1" "0"]])

  (map #(string/split % #"") sample-input)

  (def transposed (apply transpose
                         (map #(string/split % #"") sample-input)))

  (map frequencies transposed))

(defn get-gamma-epsilon-in-map [m type]
  (let [zero (get m "0")
        one (get m "1")]
    (if (= :gamma type)
      (if (> zero one) "0" "1")
      (if (> zero one) "1" "0"))))

(defn get-gamma-epsilon [input]
  (->> input
       (map #(string/split % #""))
       (apply transpose)
       (map frequencies)))

(defn get-gamma-value [freq]
  (->> freq
       (map #(get-gamma-epsilon-in-map % :gamma))
       (#(string/join %))
       (#(Integer/parseInt % 2))))

(defn get-epsilon-value [freq]
  (->> freq
       (map #(get-gamma-epsilon-in-map % :epsilon))
       (#(string/join %))
       (#(Integer/parseInt % 2))))

(defn get-gamma-epsilon-value [freq]
  [(get-gamma-value freq) (get-epsilon-value freq)])

(->> sample-input
     get-gamma-epsilon
     get-gamma-epsilon-value)

(get-gamma-epsilon sample-input)

(defn part-one-solution [input]
  (apply * (->> input
                get-gamma-epsilon
                get-gamma-epsilon-value)))

(def part-one-input (->> "resources/day03/part_one.txt"
                         slurp
                         clojure.string/split-lines))

(part-one-solution sample-input)
(part-one-solution part-one-input) ;;2724524

(defn get-more-bit-for-oxygen-co2 [xs n type]
  (->> xs
       (map #(string/split % #""))
       (apply transpose)
       (map frequencies)
       (#(nth % n))
       (#(let [zero (get % "0")
               one (get % "1")]
           (if (= :co2 type)
             (if (>= one zero) "0" "1")
             (if (> zero one) "0" "1"))))))

(defn filter-input [xs n type]
  (let [more-bit (get-more-bit-for-oxygen-co2 xs n type)]
    (filter #(= more-bit (nth (string/split % #"") n)) xs)))

(defn get-oxygen-co2-rating [xs n type]
  (if (= 1 (count xs))
    (first xs)
    (get-oxygen-co2-rating (filter-input xs n type) (inc n) type)))

(defn get-oxygen-co2 [input]
  (let [oxygen (get-oxygen-co2-rating input 0 :co2)
        co2 (get-oxygen-co2-rating input 0 :oxygen)]
    (->> [oxygen co2]
         (map #(string/join %))
         (map #(Integer/parseInt % 2)))))

(defn part-two-solution [input]
  (apply * (get-oxygen-co2 input)))

(part-two-solution sample-input)
(part-two-solution part-one-input) ;;2775870