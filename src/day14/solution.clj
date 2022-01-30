(ns day14.solution
  (:require [clojure.string :as string]))

(def sample-input "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

sample-input

(def part-one-input (slurp "resources/day14/part_one.txt"))

part-one-input

(defn parse-input [input]
  (let [[polymer-template pairs-str] (string/split input #"\n\n")
        pairs (->> pairs-str
                   string/split-lines
                   (map #(string/split % #" -> ")))]
    [polymer-template pairs]))

(parse-input sample-input)

(defn insert-in-between [pair element]
  (apply str (interpose element (string/split pair #""))))

(defn match-insert [pair element template]
  (if (= template pair) (insert-in-between pair element)
      template))

(defn get-pairs-from-polymer [polymer]
  (->> (string/split polymer #"")
       (partition 2 1)
       (map #(apply str %))))


(defn drop-one-char [element]
  (apply str (drop 1 (string/split element #""))))

(defn join-pairs-into-polymer [pairs]
  (let [[first & rest] pairs
        rest-dropped (->> rest
                          (map drop-one-char))]
    (apply str (conj rest-dropped first))))

(defn apply-one-pair-in-template-pairs [pair element template-pairs]
  (map (partial match-insert pair element) template-pairs))

(defn apply-one-pair [pair element template]
  (apply-one-pair-in-template-pairs pair element (get-pairs-from-polymer template)))

(defn apply-all-pairs [pairs template-pairs]
  (if (empty? pairs)
    template-pairs
    (let [[pair element] (first pairs)]
      (recur (rest pairs) (apply-one-pair-in-template-pairs pair element template-pairs)))))

(defn perform-one-step [template pairs]
  (join-pairs-into-polymer (apply-all-pairs pairs (get-pairs-from-polymer template))))

(defn perform-n-steps [template pairs n]
  (if (zero? n)
    template
    (recur (perform-one-step template pairs) pairs (dec n))))

(defn calc-diff [freq-list]
  (let [f (first freq-list)
        l (last freq-list)]
    (- (val l) (val f))))

(defn get-part-one-diff [template]
  (->> (string/split template #"")
       frequencies
       (sort-by val)
       calc-diff))

(defn part-one-solution [input]
  (let [[template pairs] (parse-input input)]
    (get-part-one-diff (perform-n-steps template pairs 10))))

(part-one-solution sample-input)
(part-one-solution part-one-input)

(defn get-double-counted-freq [pairs-freq first-letter last-letter]
  (let [first-letter-map (reduce (partial merge-with +) (map (fn [[k v]]
                                                               {(str (first k)) v}) pairs-freq))
        second-letter-map (reduce (partial merge-with +) (map (fn [[k v]]
                                                                {(str (last k)) v}) pairs-freq))]
    (merge-with + first-letter-map second-letter-map {first-letter 1} {last-letter 1})))

(defn get-new-pairs [pair element]
  (map #(apply str %) (partition 2 1 (interpose element (string/split pair #"")))))

(defn get-polymer-pair-delta [[pair element] delta original]
  (if (get original pair)
    (let [new-pairs (get-new-pairs pair element)]
      (merge-with +
                  (into {} (map (fn [k]
                                  [k (get original pair)]) new-pairs))
                  delta
                  {pair (* -1 (get original pair))}))
    delta))

(defn update-pairs-frequency [pairs-freq polymer-pairs]
  (let [delta (loop [polymer-pairs polymer-pairs
                     delta {}]
                (if (empty? polymer-pairs)
                  delta
                  (recur (rest polymer-pairs) (get-polymer-pair-delta (first polymer-pairs) delta pairs-freq))))]
    (into {} (filter #(pos? (val %)) (merge-with + pairs-freq delta)))))

(defn get-result-frequency [pairs-freq polymer]
  (get-double-counted-freq
   pairs-freq
   (str (first polymer))
   (str (last polymer))))

(defn calc-diff-part-two [freq-list]
  (let [[_ f] (first freq-list)
        [_ l] (last freq-list)]
    (/ (- l f) 2)))

(defn part-two-solution [input loop-count]
  (let [[polymer polymer-pairs] (parse-input input)
        final-freq (loop [n loop-count
                          pairs-freq (frequencies (get-pairs-from-polymer polymer))]
                     (if (= 0 n)
                       pairs-freq
                       (recur (dec n) (update-pairs-frequency pairs-freq polymer-pairs))))]
    (calc-diff-part-two (sort-by val (get-result-frequency final-freq polymer)))))

(part-two-solution sample-input 10)
(part-two-solution sample-input 40)
(part-two-solution part-one-input 10)
(part-two-solution part-one-input 40)

(comment
  (frequencies (get-pairs-from-polymer "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"))
  (frequencies (get-pairs-from-polymer "NBBBCNCCNBBNBNBBCHBHHBCHB"))
  (frequencies (get-pairs-from-polymer "NBCCNBBBCBHCB"))
  (frequencies (get-pairs-from-polymer "NCNBCHB"))
  (string/split sample-input #"\n\n")
  (string/includes? "AB" "C")

  (get-pairs-from-polymer "NNCB")

  (let [[template pairs] (parse-input sample-input)]
    (loop [original template
           template template
           pairs pairs]
      (if (empty? pairs) original
          (let [[pair element] (first pairs)]
            (recur (apply-one-pair pair element template) original (rest pairs))))))

  (match-insert "NN" "C" "NN")

  (apply str (interpose "C" (string/split "NN" #"")))

  (let [first-letter-map (reduce (partial merge-with +) (map (fn [[k v]]
                                                               {(str (first k)) v}) (frequencies original-pairs)))
        second-letter-map (reduce (partial merge-with +) (map (fn [[k v]]
                                                                {(str (last k)) v}) (frequencies original-pairs)))]
    (merge-with + first-letter-map second-letter-map {"N" 1} {"B" 1}))


  (let [[polymer-template pairs-str] (string/split sample-input #"\n\n")
        pairs (->> pairs-str
                   string/split-lines
                   (map #(string/split % #" -> ")))]
    [polymer-template pairs]))