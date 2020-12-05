(ns day4-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]
            [day4]))

(deftest part1-parse-portfolio-str-test
  (is (= {"aaa" "bbb",
          "ccc" "ddd"}
         (day4/map<-portfolio-str "aaa:bbb ccc:ddd"))))

(deftest part1-parse-portfolios-test
  (let [lines
        ["aaa:bbb ccc:ddd"
         "eee:fff"
         ""
         "aaa:bbb"
         ""
         "aaa:bbb"
         "ccc:ddd"]]
        (is (= [{"aaa" "bbb", "ccc" "ddd", "eee" "fff"}
                {"aaa" "bbb"}
                {"aaa" "bbb", "ccc" "ddd"}]
               (day4/parse-portfolios lines)))))

(deftest part2-specs-test
  (is (true? (spec/valid? :day4/byr "2002")))
  (is (false? (spec/valid? :day4/byr "2003")))
  (is (true? (spec/valid? :day4/hgt "60in")))
  (is (true? (spec/valid? :day4/hgt "190cm")))
  (is (false? (spec/valid? :day4/hgt "190in")))
  (is (false? (spec/valid? :day4/hgt "190")))
  (is (true? (spec/valid? :day4/hcl "#123abc")))
  (is (false? (spec/valid? :day4/hcl "#123abz")))
  (is (false? (spec/valid? :day4/hcl "123abz")))
  (is (true? (spec/valid? :day4/ecl "brn")))
  (is (false? (spec/valid? :day4/ecl "wat")))
  (is (true? (spec/valid? :day4/pid "000000001")))
  (is (false? (spec/valid? :day4/pid "0123456789")))
  )


(deftest part2-portfolio-valid-test
  (testing "with invalid portfolios"
    (is (false? (spec/valid? :day4/portfolio2
                             (day4/map<-portfolio-str "eyr:1972 cid:100 hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"))))
    (is (false? (spec/valid? :day4/portfolio2
                             (day4/map<-portfolio-str "iyr:2019 hcl:#602927 eyr:1967 hgt:170cm ecl:grn pid:012533040 byr:1946"))))
    (is (false? (spec/valid? :day4/portfolio2
                             (day4/map<-portfolio-str "hcl:dab227 iyr:2012 ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277")))))
  (testing "with valid portfolios"
    (is (spec/valid? :day4/portfolio2
                     (day4/map<-portfolio-str "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f")))
    (is (spec/valid? :day4/portfolio2
                     (day4/map<-portfolio-str "eyr:2029 ecl:blu cid:129 byr:1989 iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm")))))
