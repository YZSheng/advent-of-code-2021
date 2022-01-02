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

sample-input

(def part-one-input (->> "resources/day08/part_one.txt"
                         slurp))

part-one-input

(string/split-lines sample-input)

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