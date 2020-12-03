(ns day3
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.pprint :refer [pprint]]))

;; COMMON
(defn tree?
  "Returns true if there is a tree at this line with the given slope"
  [line index]
  (= \# (nth line (mod index (count line)))))

(defn count-trees [[x y]]
  (with-open [rdr (io/reader "./inputs/day3.txt")]
    (loop [trees 0,
           remaining-lines (line-seq rdr),
           pos  0]
      (if-let [line (first remaining-lines)]
        (recur (cond-> trees (tree? line pos) inc)
               (nthrest remaining-lines y)
               (+ pos x))
        trees))))


(defn -main []
  (pprint [(count-trees [3 1])
           (apply * (map count-trees [[1 1] [3 1] [5 1] [7 1] [1 2]]))]))
