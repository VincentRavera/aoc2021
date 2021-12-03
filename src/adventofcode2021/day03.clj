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
    (if (> num_of_ones num_of_zeros)
      1
      0)))


(defn computeColumn
  ""
  [collnumber]
  (fn [lines]
    (->> lines
       (map #(nth % collnumber))
       arrayMostCommon)))


(defn computeGamma
  ""
  [lines]
  (let [numbits (count (first lines))
        gammabits (->> (range numbits)
                       (map computeColumn)
                       (map #(apply % (list lines))))
        ]
    gammabits
    )
  )

(defn bitarray2decimal
  ""
  [x]
  (Integer/parseInt (str/join "" x) 2))

(defn part1
  ""
  [path]
  (let [gammabytes  (->> path
                         parse
                         isolateBit
                         computeGamma)
        epsilonbytes (map myxor gammabytes)
        gamma (bitarray2decimal gammabytes)
        epsilon (bitarray2decimal epsilonbytes)
        ]
    (prn 'gammab gammabytes)
    (prn 'epsilonb epsilonbytes)
    (prn 'gamma gamma)
    (prn 'epsilon epsilon)
    (* gamma epsilon)))


(defn start
  ""
  []
  (let [test (part1 "resources/day03/test")
        real (part1 "resources/day03/real")]
    (println (format "Test is at %s : %s" test (= 198 test)))
    (println (format "Answer is: %s" real))
    real)
  )
