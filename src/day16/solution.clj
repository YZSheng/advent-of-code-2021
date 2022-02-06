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
    [(convert-binary-to-decimal (string/join "" four-bits)) (* 5 (count four-bits))]))

(defn parse-by-type [type-id bits]
  (case type-id
    4 (let [[value used] (get-literal-value bits)]
        [{:value value} used])
    (let [[value used] (parse-nested bits)]
      [{:nested value} used])))

(defn parse-bits [binary]
  (if (empty? binary)
    [{} 0]
    (let [packet-version (get-version-number binary)
          type-id (get-type-id binary)
          rest-bits (get-rest-bits binary)
          [parsed used] (parse-by-type type-id rest-bits)]
      [(assoc parsed :packet-version packet-version :type-id type-id) (+ used 6)])))


(defn parse-nested-mode-zero [bit-char-list]
  (let [sub-packets-length (convert-binary-to-decimal (string/join "" (take 15 bit-char-list)))
        rest-chars (drop 15 bit-char-list)]
    (loop [rest-chars rest-chars
           sub-packets-length sub-packets-length
           sub-packets []
           used 0]
      (if (zero? sub-packets-length)
        [sub-packets used]
        (let [[parse-result used-bits] (parse-bits rest-chars)]
          (recur (drop used-bits rest-chars)
                 (- sub-packets-length used-bits)
                 (conj sub-packets parse-result)
                 (+ used used-bits)))))))

(defn parse-nested-mode-one [bit-char-list]
  (let [sub-packets-count (convert-binary-to-decimal (string/join "" (take 11 bit-char-list)))
        rest-chars (drop 11 bit-char-list)]
    (loop [rest-chars rest-chars
           sub-packets-count sub-packets-count
           sub-packets []
           used 0]
      (if (zero? sub-packets-count)
        [sub-packets used]
        (let [[parse-result used-bits] (parse-bits rest-chars)]
          (recur (drop used-bits rest-chars)
                 (dec sub-packets-count)
                 (conj sub-packets parse-result)
                 (+ used used-bits)))))))

(defn parse-nested [bits]
  (if (= (first bits) \0)
    (parse-nested-mode-zero (drop 1 bits))
    (parse-nested-mode-one (drop 1 bits))))

(defn parse-hex-input [hex-input]
  (parse-bits (convert-hex-to-binary hex-input)))

(defn sum-versions [parsed]
  (let [without-nested (dissoc parsed :nested)
        nested (:nested parsed)]
    (loop [flat [without-nested]
           nested nested]
      (if (empty? nested)
        flat
        (recur (concat flat (map #(dissoc % :nested) nested))
               (flatten (map :nested nested)))))))

(comment
  (convert-hex-to-binary "D2FE28")
  (convert-hex-to-binary "8A004A801A8002F478")
  (convert-binary-to-decimal "110")
  (get-version-number "110100101111111000101000")
  (get-type-id "110100101111111000101000")
  (get-rest-bits "110100101111111000101000")
  (get-literal-value "101111111000101000")

  (parse-by-type 4 "101111111000101000")

  (parse-bits "110100101111111000101000")

  (parse-nested-mode-zero "0000000000110111101000101001010010001001000000000")

  (parse-nested-mode-one "0000000001101010000001100100000100011000001100000")

  (parse-bits "11101110000000001101010000001100100000100011000001100000")
  (parse-bits "110100010100101001000100100")

  (parse-hex-input "38006F45291200")
  (parse-hex-input "EE00D40C823060")
  (parse-hex-input "8A004A801A8002F478")

  (parse-hex-input "620080001611562C8802118E34")

  (parse-hex-input "C0015000016115A2E0802F182340")
  (sum-versions (first (parse-hex-input "8A004A801A8002F478")))

  (map parse-hex-letter (string/split "D2FE28" #"")))


