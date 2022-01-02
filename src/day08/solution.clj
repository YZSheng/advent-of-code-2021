(ns day08.solution
  (:require [clojure.string :as string]))

(def sample-input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(def part-one-input (slurp "resources/day08/part_one.txt"))

(defn is-one [segment]
  (= 2 (count segment)))

(defn is-four [segment]
  (= 4 (count segment)))

(defn is-seven [segment]
  (= 3 (count segment)))

(defn is-eight [segment]
  (= 7 (count segment)))

(defn part-one-solution [input]
  (->> input
       string/split-lines
       (map #(string/split % #"\s\|\s"))
       (map second)
       (map #(string/split % #"\s"))
       flatten
       (filter #(or (is-one %) (is-four %) (is-seven %) (is-eight %)))
       count))

(part-one-solution sample-input)
(part-one-solution part-one-input)

(defn find-three [len-five-segments one]
  (first (filter #(= 2 (count (clojure.set/intersection
                               (set one)
                               (set %)))) len-five-segments)))

(defn find-two [two-or-five four]
  (first (filter #(= 2 (count (clojure.set/intersection
                               (set four)
                               (set %)))) two-or-five)))

(defn sort-alphabetically [segments]
  (map string/join (map sort (map #(string/split % #"") segments))))

(defn find-nine [len-six-segments four]
  (first (filter #(= 0 (count (clojure.set/difference
                               (set four)
                               (set %)))) len-six-segments)))

(defn find-zero [zero-or-six one]
  (first (filter #(= 0 (count (clojure.set/difference
                               (set one)
                               (set %)))) zero-or-six)))

(defn decode-input-entry [input-entry]
  (let [segments-by-len (group-by count (sort-alphabetically input-entry))
        one (first (get segments-by-len 2))
        four (first (get segments-by-len 4))
        seven (first (get segments-by-len 3))
        eight (first (get segments-by-len 7))
        len-five-segments (set (get segments-by-len 5))
        three (find-three len-five-segments one)
        two-or-five (set (remove #{three} len-five-segments))
        two (find-two two-or-five four)
        five (first (remove #{two} two-or-five))
        len-six-segments (set (get segments-by-len 6))
        nine (find-nine len-six-segments four)
        zero-or-six (set (remove #{nine} len-six-segments))
        zero (find-zero zero-or-six one)
        six (first (remove #{zero} zero-or-six))]
    {zero 0
     one 1
     two 2
     three 3
     four 4
     five 5
     six 6
     seven 7
     eight 8
     nine 9}))

(defn get-output-value [input output]
  (let [sorted-output (sort-alphabetically output)]
    (Integer/parseInt (string/join (map #(get (decode-input-entry input) %) sorted-output)))))

(defn get-output-value-by-line [line]
  (let [[i o] (string/split line #"\s\|\s")
        input (string/split i #"\s")
        output (string/split o #"\s")]
    (get-output-value input output)))

(defn part-two-solution [input]
  (->> input
       string/split-lines
       (map get-output-value-by-line)
       (reduce +)))

(part-two-solution sample-input)
(part-two-solution part-one-input)