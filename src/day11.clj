(ns day11
  (:require
   [clojure.string :as str]))

(def data
  (str/split-lines (slurp "./inputs/day11.txt")))

(defn indexed-matrix [matrix]
  (into {} (for [[y row]  (map-indexed vector matrix)
                 [x cell] (map-indexed vector row)]
             {[x y] cell})))

(defn mutate [seat-selector taken-threshold imat]
  (into {} (map
            (fn [[pos cell]]
              (let [taken (count (filter #{\#} (seat-selector imat pos)))]
                [pos (cond
                       (and (= \L cell) (zero? taken)) \#
                       (and (= \# cell) (<= taken-threshold taken)) \L
                       :else cell)]))
            imat)))

(defn mature [seat-selector taken-threshold imat]
  (let [mutated (mutate seat-selector taken-threshold imat)]
    (if (= mutated imat)
      imat
      (recur seat-selector taken-threshold mutated))))

(defn count-taken [imat]
  (count (filter #{\#} (vals imat))))

(def movers
  (for [ymv #{dec identity inc},
        xmv #{dec identity inc},
        :when (not= [xmv ymv] [identity identity])]
    (fn [[x y]] [(xmv x) (ymv y)])))

(defn cells-around [imat pos]
  (for [move movers] (get imat (move pos) nil)))

(defn cells-visible [imat pos]
  (for [move movers
        :let [ray (rest (iterate move pos))]]
    (->> (map imat ray)     ;; Get cells
         (take-while some?) ;; Stop when outOfBound (returns nil)
         (some #{\# \L})))) ;; Stop at first found

(defn part1 [raw]
  (count-taken (mature cells-around 4 (indexed-matrix raw))))

(defn part2 [raw]
  (count-taken (mature cells-visible 5 (indexed-matrix raw))))

(println [(part1 data) (part2 data)])
