(ns day10.solution
  (:require [clojure.string :as string]))

(def sample-input "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

sample-input

(def part-one-input (slurp "resources/day10/part_one.txt"))
part-one-input

(defn parse-input [input]
  (string/split-lines input))

(defn strip [s]
  (-> s
      (string/replace #"<>" "")
      (string/replace #"\{\}" "")
      (string/replace #"\[\]" "")
      (string/replace #"\(\)" "")))

(defn strip-all [s]
  (if (some #(string/includes? s %) ["<>" "{}" "[]" "()"])
    (strip-all (strip s))
    s))

(defn find-illegal-char [s]
  (->> s
       strip-all
       (#(string/split % #""))
       (remove empty?)
       (filter #(string/includes? ">]})" %))
       first))

(defn get-points [char]
  (case char
    ")" 3
    "]" 57
    "}" 1197
    ">" 25137))

(defn part-one-solution [input]
  (->> input
       parse-input
       (map find-illegal-char)
       (remove nil?)
       (map get-points)
       (reduce +)))

(part-one-solution sample-input)
(part-one-solution part-one-input)

(comment

  (def incomplete "[({(<(())[]>[[{[]{<()<>>")
  (def complete "(((((((((())))))))))")
  (def corrupted "{([(<{}[<>[]}>{[]{[(<()>")

  (strip-all incomplete)
  (strip-all complete)
  (strip-all corrupted)

  (find-illegal-char corrupted)
  (find-illegal-char incomplete)
  (find-illegal-char complete)
  (find-illegal-char "[[<[([]))<([[{}[[()]]]")
  (find-illegal-char "[{[{({}]{}}([{[{{{}}([]")
  (find-illegal-char "<{([([[(<>()){}]>(<<{{"))