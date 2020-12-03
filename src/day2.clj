(ns day2
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.pprint :refer [pprint]]))

;; PART 1

(defn line-valid1? [line]
  (let [[interval letter pwd] (str/split line #" ")
        [min max] (mapv read-string (str/split interval #"-"))
        ch (first letter)
        occurences  (count (filter #{ch} pwd))]
    (<= min occurences max)))

(defn result1 []
  (with-open [rdr (io/reader "./inputs/day2.txt")]
    (->> (line-seq rdr)
      (filter line-valid1?)
      count)))

;; PART 2

(defn line-valid2? [line]
  (let [[nbs letter pwd] (str/split line #" ")
        [a b] (mapv read-string (str/split nbs #"-"))
        c             (first letter)
        char-matches? #(= (nth pwd (dec %)) c)
        single-match? (not= (char-matches? a) (char-matches? b))]
    single-match?))

(defn result2 []
  (with-open [rdr (io/reader "./inputs/day2.txt")]
    (->> (line-seq rdr)
      (filter line-valid2?)
      count)))

(defn -main [] (pprint [(result1) (result2)]))
