(ns day18
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]))

(def data
  (str/split-lines (slurp "inputs/day18.txt")))

(defn infix [[left op right]]
  ((eval op)
   (cond-> left (coll? left) (infix))
   (cond-> right (coll? right) (infix))))

(defn wrap-line [line]
  (str \( line \)))

(defn wrap-left-then-right [expr]
  (if-not (and (coll? expr) (< 3 (count expr)))
    expr
    (let [[left op right & rems] expr]
      (recur (cons (list left op right) rems)))))

(defn wrap-plus-then-times [expr]
  (if-not (and (coll? expr) (< 3 (count expr)))
    expr
    (let [plus-idx (.indexOf expr (symbol "+"))
          op-idx (if (pos? plus-idx) plus-idx 1)
          [left right] ((juxt dec inc) op-idx)
          wrapped (map (partial nth expr) [left op-idx right])]
      (recur (concat (take left expr) [wrapped] (drop (inc right) expr))))))

(defn calc [rule data]
  (reduce + (map (comp infix (partial walk/postwalk rule) read-string wrap-line) data)))

(prn [(calc wrap-left-then-right data)
      (calc wrap-plus-then-times data)])
