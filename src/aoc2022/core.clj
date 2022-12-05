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

(defn -main
  "Advent of Code 2022."
  [& args]
  (let [input (read-chunked-integer-input "resources/input_1.txt")]
    (println "1.1 max number of calories = " (calc-max-calories input))
    (println "1.2 top three elves' calories = " (calc-top-three-calories input))
    )
  )
