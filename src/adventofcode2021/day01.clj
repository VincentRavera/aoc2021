(ns adventofcode2021.day01
  (:require [clojure.string :as str]))

(defn diff
  ""
  [X y]
  (let [x (first X)
        c (last X)]
    ;; (prn x y '- c)
    (if (< x y)
      (list y (inc c))
      (list y c))))

(defn solve
  ""
  [input]
  (last (reduce diff
          (list (first input) 0)
          input)))
(defn pack
  ""
  [X y]
  (let [accu (first X)
        reste (last X)]
    (if (< (count accu) 3)
      (list (conj accu y) reste)
      (list (conj (reverse (rest (reverse accu) )) y) (conj reste (reduce + accu)))
      ))

  )
(defn transf
  ""
  [input]
  (let [x (reduce pack '(nil nil) input)
        y (conj (last x) (reduce + (first x)))]
    (prn y)
    (reverse y)
    )

  )

(defn parse
  ""
  [path]
  (map read-string (str/split-lines (slurp path))))


(defn start
  ""
  []
  (let [test_data (parse "resources/day01/test")
        real_data (parse "resources/day01/real")
        test (solve test_data)
        real (solve real_data)
        test2 (solve (transf test_data))
        real2 (solve (transf real_data))]

    (println (format "Test is at %s : %s" test (= 7 test)))
    (println (format "Answer is: %s" real))
    (println (format "Test is at %s : %s" test2 (= 5 test2)))
    (println (format "Answer is: %s" real2))
    real))
