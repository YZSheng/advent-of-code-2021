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

(defn matching-char [char]
  (case char
    "{" "}"
    "(" ")"
    "[" "]"
    "<" ">"))

(defn is-incomplete [s]
  (let [dangling-chars (->> s
                            strip-all
                            (#(string/split % #""))
                            (remove empty?))]
    {:chars dangling-chars
     :incomplete (and
                  (every? #(string/includes? "([{<" %) dangling-chars)
                  (pos? (count dangling-chars)))}))

(defn get-missing-matching-string [s]
  (->> "[({(<(())[]>[[{[]{<()<>>"
       is-incomplete
       :chars
       reverse
       (map matching-char)
       (string/join)))

(defn get-char-completion-score [char]
  (case char
    ")" 1
    "]" 2
    "}" 3
    ">" 4))

(defn get-completion-score [s]
  (loop [chars (string/split s #"")
         score 0]
    (if (empty? chars) score
        (recur (rest chars) (+ (* 5 score) (get-char-completion-score (first chars)))))))

(defn part-two-solution [input]
  (let [scores (->> input
                    parse-input
                    (filter #(:incomplete (is-incomplete %)))
                    (map get-missing-matching-string)
                    (map get-completion-score))
        sorted (sort scores)
        l (count scores)]
    (nth sorted (/ (dec l) 2))))

(part-two-solution sample-input)
(part-two-solution part-one-input)