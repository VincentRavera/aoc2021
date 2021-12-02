(ns adventofcode2021.day02
  (:require [clojure.string :as str]))

(defn inputtopair
  ""
  [input]
  (let [x (str/split input #" ")]
    (vector (first x) (read-string (last x)))))
(defn generatecoord
  ""
  [direction amount]
  (cond (= "forward" direction) (fn [x y] (list (+ x amount) y))
        (= "down" direction) (fn [x y] (list x (+ y amount)))
        (= "up" direction) (fn [x y] (list x (- y amount)))))


(defn computecoord
  ""
  [x y]
  (apply y x))

(defn solvep1
  ""
  [input]
  (->> input
       (map inputtopair)
       (map #(apply generatecoord %))
       (reduce computecoord '(0 0))
       ))

(defn generatecoordp2
  ""
  [direction amount]
  (cond (= "forward" direction) (fn [x y z] (list (+ x amount) (+ y (* z amount)) z))
        (= "down" direction) (fn [x y z] (list x y (+ z amount)))
        (= "up" direction) (fn [x y z] (list x y (- z amount)))))


(defn solvep2
  ""
  [input]
  (->> input
       (map inputtopair)
       (map #(apply generatecoordp2 %))
       (reduce computecoord '(0 0 0))
       ))

(defn parse
  ""
  [path]
  (str/split-lines (slurp path)))

(defn p2Final
  ""
  [x y z]
  (* x y))

(defn start
  ""
  []
  (let [test_data (parse "resources/day02/test")
        real_data (parse "resources/day02/real")
        test (solvep1 test_data)
        real (solvep1 real_data)
        test2 (solvep2 test_data)
        real2 (solvep2 real_data)]

    (println (format "Test is at %s : %s" test (= 150 (apply * test))))
    (println (format "Answer is %s: %s" real (apply * real)))
    (println (format "Test is at %s : %s" test2 (= 900 (apply p2Final test2))))
    (println (format "Answer is %s: %s" real2 (apply p2Final real2)))
    (apply * test)))
