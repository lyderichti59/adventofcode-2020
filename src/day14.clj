(ns day14
  (:require [clojure.string :as str]))

(def data
  (str/split-lines (slurp "./inputs/day14.txt")))

(def blocks
  (->> (partition-by (partial take 2) data) ;; Partition lines by first two letters
       (partition 2)                        ;; Gathers mask-line seq with mem-lines seq
       (mapv (partial apply concat))))      ;; Concatenate pair of seqs into a single vector

(defn extract-mem [line]
  (mapv #(Integer/parseInt %) (str/split (subs line 4) #"] = ")))

(defn parse-block-into [encoder global-mem [mask-line & mem-lines]]
  (let [indexed-mask (map-indexed vector (reverse (subs mask-line 7)))
        raw-mems (map extract-mem mem-lines)]
    (into global-mem (mapcat (partial encoder indexed-mask) raw-mems))))

(defn calc [encoder blocks]
  (reduce + (vals (reduce (partial parse-block-into encoder) {} blocks))))

;;; Part 1
(defn apply-bitmask-part1 [number [nth-bit bitmask]]
  (case bitmask
    \X number,
    \1 (bit-set number nth-bit)
    \0 (bit-clear number nth-bit)))

(defn encoder-part1 [indexed-mask [addr v]]
  [[addr (reduce apply-bitmask-part1 v indexed-mask)]])

;;; Part 2
(defn apply-bitmask-part2 [numbers [nth-bit bitmask]]
  (case bitmask
    \0 numbers
    \1 (map #(bit-set % nth-bit) numbers)
    \X (concat numbers (map #(bit-flip % nth-bit) numbers))))

(defn encoder-part2 [indexed-mask [addr v]]
  (map vector
       (reduce apply-bitmask-part2 [addr] indexed-mask)
       (repeat v)))

(prn [(calc encoder-part1 blocks)
      (calc encoder-part2 blocks)])
