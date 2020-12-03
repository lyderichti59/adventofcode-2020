(ns day3
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.pprint :refer [pprint]]))

;; COMMON
(defn tree?
  "Returns true if there is a tree at this line with the given slope"
  [[index line]]
  (= \# (nth line (mod index (count line)))))

(defn count-trees [[x y]]
  (with-open [rdr (io/reader "./inputs/day3.txt")]
    (let [x-positions (iterate (partial + x) 0)], ;;Sequence of x positions
      (->> (line-seq rdr)
        (take-nth y)              ;;Skip lines y by y
        (map vector x-positions)  ;;zip x position with each line
        (filter tree?)            ;;Keep lines with trees
        count))))                 ;;Count

(defn -main []
  (pprint [(count-trees [3 1])
           (apply * (map count-trees [[1 1] [3 1] [5 1] [7 1] [1 2]]))]))
