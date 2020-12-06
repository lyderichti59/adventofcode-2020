(ns day6
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set]))

                                        ; PART 1
(def result
  (with-open [rdr (io/reader  "./inputs/day6.txt")]
    (let [lines (line-seq rdr)
          groups (->> lines
                      (partition-by s/blank?)
                      (remove (comp s/blank? first))
                      (map (partial map set)))
          yes-per-groups (map clojure.set/union groups)
          result1 (reduce + (map count yes-per-groups))
          common-yes (map (partial apply clojure.set/intersection) groups)
          result2 (reduce + (map count common-yes))]
      [result1 result2])))


(println result)
