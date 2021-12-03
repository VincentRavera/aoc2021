(ns adventofcode2021.day03
  (:require [clojure.string :as str]))


(defn parse
  ""
  [path]
  (str/split-lines (slurp path)))


(defn isolateBit
  ""
  [input]
  (->> input
       (map #(str/split % #""))
       (map #(map read-string %))
       ))

(def test_data (->> "resources/day03/test"
                    parse
                    isolateBit))

(def real_data (->> "resources/day03/real"
                    parse
                    isolateBit))

(defn myxor
  ""
  [x]
  (if (= x 1)
    0
    1))

(defn arrayMostCommon
  [array]
  (let [num_of_zeros (count (filter #(= % 0) array))
        num_of_ones (count (filter #(= % 1) array))]
    ;; TODO <= =>
    (if (>= num_of_ones num_of_zeros)
      1
      0)))

(defn arrayLeastCommon
  [array]
  (let [num_of_zeros (count (filter #(= % 0) array))
        num_of_ones (count (filter #(= % 1) array))]
    ;; TODO <= =>
    (cond (< num_of_zeros num_of_ones) 0
          (= num_of_zeros num_of_ones) 0
          :else 1)
    ))

(defn computeColumn
  ""
  [collnumber]
  (fn [lines]
    ;; (prn 'column collnumber)
    (->> lines
         (map #(nth % collnumber))
         arrayMostCommon)))

(defn computeP2Column
  ""
  [collnumber arrayXCommon]
  (fn [lines]
    (prn 'column collnumber)
    (->> lines
         (map #(nth % collnumber))
         arrayXCommon)))


(defn computeGamma
  ""
  [lines]
  (let [numbits (count (first lines))
        gammabits (->> (range numbits)
                       (map computeColumn)
                       (map #(apply % (list lines))))
        ]
    gammabits))


(defn bitarray2decimal
  ""
  [x]
  (Integer/parseInt (str/join "" x) 2))

(defn part1
  ""
  [data]
  (let [gammabytes  (->> data
                         computeGamma)
        epsilonbytes (map myxor gammabytes)
        gamma (bitarray2decimal gammabytes)
        epsilon (bitarray2decimal epsilonbytes)
        ]
    ;; (prn 'gammab gammabytes)
    ;; (prn 'epsilonb epsilonbytes)
    ;; (prn 'gamma gamma)
    ;; (prn 'epsilon epsilon)
    (* gamma epsilon)))

(defn geno2predicate
  ""
  [mostcommonbit col]
  (fn [line]
    (if (= mostcommonbit (nth line col))
      true
      nil)))

(defn o2Recur
  ""
  [lines maxrecur output]
  (let [col (- (count (first lines)) maxrecur)
        most (apply (computeP2Column col arrayMostCommon) (list lines))
        myfilter (geno2predicate most col)
        filtered_lines (filter myfilter lines)]
    (if (or (>= 1 (count filtered_lines))
            (= 0 maxrecur))
      (first filtered_lines)
      (recur filtered_lines (dec maxrecur) (conj output most))
      )
    )
  )
(defn Co2Recur
  ""
  [lines maxrecur output]
  (let [col (- (count (first lines)) maxrecur)
        most (apply (computeP2Column col arrayLeastCommon) (list lines))
        myfilter (geno2predicate most col)
        filtered_lines (filter myfilter lines)]
    (if (or (>= 1 (count filtered_lines))
            (= 0 maxrecur))
      (first filtered_lines)
      (recur filtered_lines (dec maxrecur) (conj output most))
      )
    )
  )

(defn part2
  ""
  [data]
  (let [o2byte  (o2Recur data (count (first data)) '())
        co2byte (Co2Recur data (count (first data)) '())]
    (* (bitarray2decimal o2byte) (bitarray2decimal co2byte))))

(defn start
  ""
  []
  (let [test (part1 test_data)
        real (part1 real_data)
        testp2 (part2 test_data)
        realp2 (part2 real_data)]
    (println (format "Test is at %s : %s" test (= 198 test)))
    (println (format "Answer is: %s" real))
    (println (format "Test is at %s : %s" testp2 (= 230 testp2)))
    (println (format "Answer is: %s" realp2))
    real)
  )
