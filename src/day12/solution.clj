(ns day12.solution
  (:require [clojure.string :as string]))

(def sample-input "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def part-one-input (slurp "resources/day12/part_one.txt"))

(defn parse-input [input]
  (map #(string/split % #"-") (string/split-lines input)))

(defn get-connection-containing [node input]
  (filter #(some #{node} %) (parse-input input)))

(defn is-upper-case [char]
  (= char (string/upper-case char)))

(defn get-next-nodes [current input]
  (if (= "end" current)
    []
    (let [conns (get-connection-containing current input)]
      (map #(first (remove #{current} %)) conns))))

(defn extend-path [path nodes]
  (->> nodes
       (remove #(and (not (is-upper-case %)) (some #{%} path)))
       (map #(conj path %))))

(defn get-possible-extended-paths [path input]
  (let [next-nodes (get-next-nodes (last path) input)]
    (extend-path path next-nodes)))

(defn get-potential-paths [paths]
  (->> paths
       (remove #(= "end" (last %)))))

(defn is-valid-path [path]
  (= "end" (last path)))


(defn find-all-paths [input]
  (loop [paths [["start"]]
         valid-paths []]
    (let [extended-paths (mapcat #(get-possible-extended-paths % input) paths)]
      (if (empty? extended-paths)
        valid-paths
        (recur extended-paths (concat valid-paths (filter is-valid-path extended-paths)))))))

(defn part-one-solution [input]
  (count (find-all-paths input)))

(part-one-solution sample-input)
(part-one-solution part-one-input)

(comment

  (is-upper-case "a")
  (is-upper-case "A")

  (get-connection-containing "start" sample-input)

  (extend-path ["a" "b"] ["c" "a" "d" "A"])

  (get-potential-paths [["start"] ["start" "a"] ["start" "a" "end"]])
  (get-possible-extended-paths ["start" "A"] sample-input)
  (get-possible-extended-paths ["start" "A" "end"] sample-input)
  (get-possible-extended-paths ["start" "b" "d"] sample-input)

  (let [paths [["start" "A" "c" "A" "b" "A" "end"] ["start" "A" "b" "A" "c" "A" "end"]]
        extended-paths (mapcat #(get-possible-extended-paths % sample-input) paths)]
    extended-paths)

  (loop [paths [["start"]]
         valid-paths []]
    (let [extended-paths (mapcat #(get-possible-extended-paths % sample-input) paths)]
      (if (empty? extended-paths)
        valid-paths
        (recur extended-paths (concat valid-paths (filter is-valid-path extended-paths)))))))