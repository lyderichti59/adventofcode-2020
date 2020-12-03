(ns day2_test
  (:require [clojure.test :refer :all]
            [day2 :refer [line-valid1? line-valid2?]]))

(deftest test-day2-part1
  (is (true? (line-valid1? "1-3 a: abcde")))
  (is (false? (line-valid1? "1-3 b: cdefg")))
  (is (true? (line-valid1? "2-9 c: ccccccccc"))))


(deftest test-day2-part2
  (is (true? (line-valid2? "1-3 a: abcde")))
  (is (false? (line-valid2? "1-3 b: cdefg")))
  (is (false? (line-valid2? "2-9 c: ccccccccc"))))
