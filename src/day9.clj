(ns day9
  (:require
   [clojure.java.io :as io]))

;;; PART 1

(defn distinct-couples "" [coll k]
  (map sorted-set (repeat (count coll) k) (remove #{k} coll)))

(defn every-distinct-couples "" [coll]
  (set (mapcat distinct-couples (iterate rest coll) coll)))

(defn duo-sum-exists? "" [numseq]
  (let [preamble (take 25 numseq)
        n (nth numseq 25)
        sums (map (partial apply +) (every-distinct-couples preamble))]
    ((set sums) n)))

;;; PART 2

(defn contiguous-sum-exists? "" [sum numseq]
  (let [contiguous-sums (into {} (map-indexed vector (reductions + numseq)))
        last-hope (first (drop-while #(< (val %) sum) contiguous-sums))]
    (when (= (val last-hope) sum)
      (->> numseq
           (take (inc (key last-hope)))
           (apply sorted-set )
           ((juxt first last))
           (reduce +)))))

(with-open [rdr (io/reader "./inputs/day9.txt")]
  (let [numseq (map #(Long/parseLong %) (line-seq rdr))
        translations (take (- (count numseq) 25) (iterate rest numseq))
        invalid-translation (first (drop-while duo-sum-exists? translations))
        result1 (nth invalid-translation 25)
        result2 (some (partial contiguous-sum-exists? result1) translations)]
    (println [result1 result2])))
