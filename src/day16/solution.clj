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
  (Long/parseLong binary-str 2))

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
  (if (empty? bits)
    [nil 0]
    (let [groups (partition 5 bits)
          four-bits (loop [groups groups
                           result []]
                      (let [group (first groups)
                            decimal-value (string/join "" (rest group))]
                        (if (= \0 (first group))
                          (conj result decimal-value)
                          (recur (rest groups) (conj result decimal-value)))))]
      [(convert-binary-to-decimal (string/join "" four-bits)) (* 5 (count four-bits))])))

(defn parse-by-type [type-id bits]
  (case type-id
    4 (let [[value used] (get-literal-value bits)]
        [{:value value} used])
    (let [[value used] (parse-nested bits)]
      [{:nested (remove empty? value)} used])))

(defn parse-bits [binary]
  (if (or (empty? binary) (every? #(= % \0) binary))
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
           sub-packets []
           used 0]
      (if (= used sub-packets-length)
        [sub-packets (+ 15 sub-packets-length)]
        (let [[parse-result used-bits] (parse-bits rest-chars)]
          (recur (drop used-bits rest-chars)
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
        [sub-packets (+ 11 used)]
        (let [[parse-result used-bits] (parse-bits rest-chars)]
          (recur (drop used-bits rest-chars)
                 (dec sub-packets-count)
                 (conj sub-packets parse-result)
                 (+ used used-bits)))))))

(defn parse-nested [bits]
  (if (= (first bits) \0)
    (let [[result bit-used] (parse-nested-mode-zero (drop 1 bits))]
      [result (inc bit-used)])
    (let [[result bit-used] (parse-nested-mode-one (drop 1 bits))]
      [result (inc bit-used)])))

(defn parse-hex-input [hex-input]
  (parse-bits (convert-hex-to-binary hex-input)))

(defn sum-versions [parsed]
  (let [nested (get parsed :nested [])
        version (:packet-version parsed)]
    (if (empty? nested)
      version
      (+ version (apply + (map sum-versions nested))))))

(defn part-one-solution [input]
  (->> input
       parse-hex-input
       first
       sum-versions))

(part-one-solution (slurp "resources/day16/part_one.txt"))

(comment
  (convert-hex-to-binary "D2FE28")
  (convert-hex-to-binary "8A004A801A8002F478")
  (convert-hex-to-binary "620080001611562C8802118E34")
  (convert-hex-to-binary "A0016C880162017C3686B18A3D4780")

  (convert-binary-to-decimal "110")
  (convert-binary-to-decimal "1011011")
  (convert-binary-to-decimal "110011110100110101111101011010010111")


  (get-version-number "110100101111111000101000")
  (get-type-id "110100101111111000101000")
  (get-rest-bits "110100101111111000101000")
  (get-literal-value "101111111000101000")

  (parse-by-type 4 "101111111000101000")

  (parse-bits "110100101111111000101000")
  (parse-bits "0111000110100")

  (parse-nested-mode-zero "0000000000110111101000101001010010001001000000000")
  (parse-nested-mode-one "0000000001101010000001100100000100011000001100000")

  (parse-nested-mode-one "0000000001000000000000000000101100001000101010110001011001000100000000010000100011000111000110100")

  (parse-bits "11101110000000001101010000001100100000100011000001100000")
  (parse-bits "110100010100101001000100100")

  (parse-bits "001000100000000010000100011000111000110100")

  (parse-hex-input "38006F45291200")
  (parse-hex-input "EE00D40C823060")

  (parse-hex-input "8A004A801A8002F478")
  (parse-hex-input "620080001611562C8802118E34")
  (parse-hex-input "C0015000016115A2E0802F182340")
  (part-one-solution "C0015000016115A2E0802F182340")

  (parse-hex-input "A0016C880162017C3686B18A3D4780")
  (parse-hex-input "20546718027401204FE775D747A5AD3C3CCEEB24CC01CA4DFF2593378D645708A56D5BD704CC0110C469BEF2A4929689D1006AF600AC942B0BA0C942B0BA24F9DA8023377E5AC7535084BC6A4020D4C73DB78F005A52BBEEA441255B42995A300AA59C27086618A686E71240005A8C73D4CF0AC40169C739584BE2E40157D0025533770940695FE982486C802DD9DC56F9F07580291C64AAAC402435802E00087C1E8250440010A8C705A3ACA112001AF251B2C9009A92D8EBA6006A0200F4228F50E80010D8A7052280003AD31D658A9231AA34E50FC8010694089F41000C6A73F4EDFB6C9CC3E97AF5C61A10095FE00B80021B13E3D41600042E13C6E8912D4176002BE6B060001F74AE72C7314CEAD3AB14D184DE62EB03880208893C008042C91D8F9801726CEE00BCBDDEE3F18045348F34293E09329B24568014DCADB2DD33AEF66273DA45300567ED827A00B8657B2E42FD3795ECB90BF4C1C0289D0695A6B07F30B93ACB35FBFA6C2A007A01898005CD2801A60058013968048EB010D6803DE000E1C6006B00B9CC028D8008DC401DD9006146005980168009E1801B37E02200C9B0012A998BACB2EC8E3D0FC8262C1009D00008644F8510F0401B825182380803506A12421200CB677011E00AC8C6DA2E918DB454401976802F29AA324A6A8C12B3FD978004EB30076194278BE600C44289B05C8010B8FF1A6239802F3F0FFF7511D0056364B4B18B034BDFB7173004740111007230C5A8B6000874498E30A27BF92B3007A786A51027D7540209A04821279D41AA6B54C15CBB4CC3648E8325B490401CD4DAFE004D932792708F3D4F769E28500BE5AF4949766DC24BB5A2C4DC3FC3B9486A7A0D2008EA7B659A00B4B8ACA8D90056FA00ACBCAA272F2A8A4FB51802929D46A00D58401F8631863700021513219C11200996C01099FBBCE6285106")
  (part-one-solution "A0016C880162017C3686B18A3D4780")

  (sum-versions {:nested
                 [{:nested
                   [{:nested
                     [{:value 6, :packet-version 7, :type-id 4}
                      {:value 6, :packet-version 6, :type-id 4}
                      {:value 12, :packet-version 5, :type-id 4}
                      {:value 15, :packet-version 2, :type-id 4}
                      {:value 15, :packet-version 2, :type-id 4}]
                     :packet-version 3
                     :type-id 0}]
                   :packet-version 1
                   :type-id 0}]
                 :packet-version 5
                 :type-id 0})


  (sum-versions (first (parse-hex-input "8A004A801A8002F478")))

  (map parse-hex-letter (string/split "D2FE28" #"")))


