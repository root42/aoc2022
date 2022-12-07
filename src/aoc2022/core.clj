(ns aoc2022.core
  (:gen-class)
  (:require [clojure.string :as string]))

(defn make-integer-vector
  [string-vector]
  (apply vector (map #(Integer. %) string-vector))
  )

(defn read-chunked-integer-input
  [input-file]
  (let [input (slurp input-file)
        chunks (string/split input #"\n\n")]
    (->> chunks
         (map string/split-lines)
         (map make-integer-vector)
         (apply vector)
         )))

(defn read-input
  [input-file]
  (let [input (slurp input-file)]
    (apply vector (clojure.string/split-lines input)))
  )

(defn read-integer-input
  [input-file]
  (let [input (slurp input-file)]
    (apply vector (map #(Integer. %) (clojure.string/split-lines input))))
  )

(defn read-input-csv
  [input-file]
  (let [input (slurp input-file)]
    (map #(Integer. %) (re-seq #"[^,\n]+" input)))
  )

;; day 1
(defn calc-max-calories
  "Calculates the maximum number of calories in the elf backpacks."
  [input]
  (->> input
       (map #(apply + %))
       (apply max))
  )

(defn calc-top-three-calories
  "Calculates the calories carried by the top three elves."
  [input]
  (->> input
       (map #(apply + %))
       (sort >)
       (take 3)
       (apply +)
       ))

;; day 2
(defn rps-score
  "Calculates score of a single RPS move:
  Player 1:
  A: rock, B: paper, C: scissors
  Player 2:
  X: rock (1 point), Y: paper (2 points), Z: scissors (3 points)
  Lost game (0 points), Draw game (3 points), Won game (6 points)"
  [i1 i2]
  (let [p1 (case i1
             "A" 0
             "B" 1
             "C" 2)
        p2 (case i2
             "X" 0
             "Y" 1
             "Z" 2)
        res (- p2 p1)]
    (condp = res
      ; draw
      0 (+ p2 1 3)
      ; won
      1 (+ p2 1 6)
      -2 (+ p2 1 6)
      ; lost
      (+ p2 1)
      )
    )
  )

(defn calc-rps-score
  "Calculates the cumulative score for the rock paper scissors game."
  [input]
  (apply + (map #(apply rps-score %) input))
  )

(defn rps-cheat-score
  "Determine RPS move score, if X, Y, Z mean 'lose', 'draw', 'win'."
  [i1 i2]
  (let [p1 (case i1
             "A" 0
             "B" 1
             "C" 2)
        ; lose -> choose one less, draw -> choose same, win -> choose one more
        res (case i2
              "X" -1
              "Y" 0
              "Z" 1)
        ; actual value we use is p1 + res modulo three, as RPS is non-transitive
        p2 (mod (+ p1 res) 3)]
    ; score is player 2 plus 1
    ; plus the intended result (-1,0,1) plus 1 times three (0,3,6)
    (+ p2 1 (* 3 (+ res 1)))
    )
  )

(defn calc-rps-cheat-score
  "Calculates the cumulative score for the rock paper scissors game."
  [input]
  (apply + (map #(apply rps-cheat-score %) input))
  )

(defn -main
  "Advent of Code 2022."
  [& args]
  (let [input (read-chunked-integer-input "resources/input_1.txt")]
    (println "1.1 max number of calories = " (calc-max-calories input))
    (println "1.2 top three elves' calories = " (calc-top-three-calories input))
    )
  (let [input (map #(string/split % #" ") (read-input "resources/input_2.txt"))]
    (println "2.1 Rock-Paper-Scissors Score = " (calc-rps-score input))
    (println "2.2 Rock-Paper-Scissors Score = " (calc-rps-cheat-score input))
    )
  )
