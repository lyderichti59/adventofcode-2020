(ns day10
  (:require
   [clojure.java.io :as io]))

(def
  ^{:doc "Calculate the number of ways to chain n contiguous adapters"}
  nb-ways
  (memoize (fn [n] (condp = n
                    1 1
                    2 2
                    3 4
                    (+ (nb-ways (- n 1))
                       (nb-ways (- n 2))
                       (nb-ways (- n 3)))))))

(with-open [rdr (io/reader "./inputs/day10.txt")]
  (let [adapters (->> (line-seq rdr)
                      (mapv #(Long/parseLong %))
                      (apply sorted-set))
        input-jolt (+ 3 (last adapters))
        diffs (map - (conj adapters input-jolt) (conj adapters 0))
        ;; PART 1
        freq (frequencies diffs)
        result1 (* (freq 1) (freq 3))
        ;; PART 2
        diff-partition (partition-by identity diffs)
        one-partitions (remove (comp #{3} first) diff-partition)
        result2 (reduce * (map (comp nb-ways count) one-partitions))
        ]
    (println [result1 result2]))
  )
