(ns day16.solution
  (:require [clojure.string :as string]))

(defn parse-hex-letter [l]
  (case (str l)
    "0" "0000"
    "1" "0001"
    "2" "0010"
    "3" "0011"
    "4" "0100"
    "5" "0101"
    "6" "0110"
    "7" "0111"
    "8" "1000"
    "9" "1001"
    "A" "1010"
    "B" "1011"
    "C" "1100"
    "D" "1101"
    "E" "1110"
    "F" "1111"))

(defn convert-binary-to-decimal [binary-str]
  (Integer/parseInt binary-str 2))

(defn convert-hex-to-binary [hex]
  (->> (string/split hex #"")
       (map parse-hex-letter)
       (string/join "")))

(defn get-version-number [binary]
  (convert-binary-to-decimal (string/join "" (take 3 binary))))

(defn get-type-id [binary]
  (convert-binary-to-decimal (string/join "" (take 3 (drop 3 binary)))))

(defn get-rest-bits [binary]
  (string/join "" (drop 6 binary)))

(defn get-literal-value [bits]
  (let [groups (partition 5 bits)
        four-bits (loop [groups groups
                         result []]
                    (let [group (first groups)
                          decimal-value (string/join "" (rest group))]
                      (if (= \0 (first group))
                        (conj result decimal-value)
                        (recur (rest groups) (conj result decimal-value)))))]
    (convert-binary-to-decimal (string/join "" four-bits))))

(defn parse-by-type [type-id bits]
  (if (= 4 type-id)
    {:value (get-literal-value bits)}
    {}))

(defn parse-bits [binary]
  (let [packet-version (get-version-number binary)
        type-id (get-type-id binary)
        rest-bits (get-rest-bits binary)
        parsed (parse-by-type type-id rest-bits)]
    (assoc parsed :packet-version packet-version :type-id type-id)))

(parse-bits "110100101111111000101000")

(comment
  (convert-hex-to-binary "D2FE28")
  (convert-binary-to-decimal "110")
  (get-version-number "110100101111111000101000")
  (get-type-id "110100101111111000101000")
  (get-rest-bits "110100101111111000101000")
  (get-literal-value "101111111000101000")
  (map parse-hex-letter (string/split "D2FE28" #"")))


