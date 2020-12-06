(ns day5-test
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest is testing]]
            [day5]))

(deftest seat-id-test
  (is (= 357 (day5/seat-id [44 5]))))


(deftest parse-binary-test
  (is (= 7 (day5/parse-binary [\0 \1] (seq "111"))))
  (is (= 70 (day5/parse-binary [\F \B] (seq "BFFFBBF")))))

(deftest seat-test
  (is (= 567 (day5/seat "BFFFBBFRRR"))))
