(ns day5
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]]))

(defn exp
  ([x n] (reduce * (repeat n x)))
  ([[x n]] (exp x n)))

(def expm (memoize exp))

(defn seat-id [[row col]]
  (+ (* 8 row) col))

(defn parse-binary [[zero-char one-char] tokens]
  (let [powers (reverse (range (count tokens)))
        two-powers (map (partial expm 2) powers)
        char-to-digit {zero-char 0, one-char 1}
        bits (map char-to-digit tokens)
        heavy-bits (map * bits two-powers)]
    (reduce + heavy-bits)))

(defn seat [pass]
  (let [r (take 7 pass)
        c (take-last 3 pass)
        row (parse-binary [\F \B] (seq r))
        col (parse-binary [\L \R] (seq c))]
    (seat-id [row col])))

(def result1
  (with-open [rdr (io/reader "./inputs/day5.txt")]
    (->> (line-seq rdr)
         (map seat)
         (apply max)
         )))

(def result2
  (with-open [rdr (io/reader "./inputs/day5.txt")]
    (let  [seats (->> (line-seq rdr)
                      (map seat)
                      (apply sorted-set))
           a (apply min seats)
           b (apply max seats)]
      (first (filter (complement seats) (range a b))))))

(println [result1 result2])
