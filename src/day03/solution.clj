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


(defn get-gamma-rate-in-map [m]
  (let [zero (get m "0")
        one (get m "1")]
    (if (> zero one) "0" "1")))

(defn get-epsilon-rate-in-map [m]
  (let [zero (get m "0")
        one (get m "1")]
    (if (> zero one) "1" "0")))

(defn get-gamma [input]
  (->> input
       (map #(string/split % #""))
       (apply transpose)
       (map frequencies)
       (map get-gamma-rate-in-map)
       (#(string/join %))
       (#(Integer/parseInt % 2))))

(defn get-epsilon [input]
  (->> input
       (map #(string/split % #""))
       (apply transpose)
       (map frequencies)
       (map get-epsilon-rate-in-map)
       (#(string/join %))
       (#(Integer/parseInt % 2))))

(defn part-one-solution [input]
  (* (get-gamma input) (get-epsilon input)))

(def part-one-input (->> "resources/day03/part_one.txt"
                         slurp
                         clojure.string/split-lines))

(part-one-solution sample-input)
(part-one-solution part-one-input)

(defn get-more-bit-for-oxygen [xs n]
  (->> xs
       (map #(string/split % #""))
       (apply transpose)
       (map frequencies)
       (#(nth % n))
       (#(let [zero (get % "0")
               one (get % "1")]
           (if (> zero one) "0" "1")))))

(defn get-more-bit-for-co2 [xs n]
  (->> xs
       (map #(string/split % #""))
       (apply transpose)
       (map frequencies)
       (#(nth % n))
       (#(let [zero (get % "0")
               one (get % "1")]
           (if (>= one zero) "0" "1")))))

(get-more-bit-for-oxygen sample-input 0)

(defn filter-oxygen [xs n]
  (let [more-bit (get-more-bit-for-oxygen xs n)]
    (filter #(= more-bit (nth (string/split % #"") n)) xs)))

(defn filter-co2 [xs n]
  (let [more-bit (get-more-bit-for-co2 xs n)]
    (filter #(= more-bit (nth (string/split % #"") n)) xs)))

(defn get-oxygen-generator-rating [xs n]
  (if (= 1 (count xs))
    (first xs)
    (get-oxygen-generator-rating (filter-oxygen xs n) (inc n))))

(defn get-co2-scrubber-rating [xs n]
  (if (= 1 (count xs))
    (first xs)
    (get-co2-scrubber-rating (filter-co2 xs n) (inc n))))

(get-oxygen-generator-rating sample-input 0)
(get-co2-scrubber-rating sample-input 0)

(defn get-oxygen [input]
  (let [rating (get-oxygen-generator-rating input 0)]
    (->> rating
         (#(string/join %))
         (#(Integer/parseInt % 2)))))

(defn get-co2 [input]
  (let [rating (get-co2-scrubber-rating input 0)]
    (->> rating
         (#(string/join %))
         (#(Integer/parseInt % 2)))))

(get-oxygen sample-input)
(get-co2 sample-input)

(defn part-two-solution [input]
  (let [oxygen (get-oxygen input)
        co2 (get-co2 input)]
    (* oxygen co2)))

(part-two-solution sample-input)
(part-two-solution part-one-input)