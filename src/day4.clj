(ns day4
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.spec.alpha :as spec]
            [clojure.pprint :refer [pprint]]
            [clojure.walk :refer [keywordize-keys]]))

;; UTILS
(defn parse-number
  "Reads a number from a string. Returns nil if not a number."
  [s]
  (if (re-find #"^-?\d+\.?\d*$" s)
    (read-string s)))

(def fields #{:byr :iyr :eyr :hgt :hcl :ecl :pid})

(defn map<-portfolio-str
  "Transforms 'aaa:bbb ccc:ddd' to {'aaa' 'bbb','ccc' 'ddd'}"
  [portfolio]
  (keywordize-keys (into {}
                         (->> (s/split portfolio #"[\s\r\n]")
                              (map #(s/split % #":"))))))

(defn parse-portfolios [lines]
  (->> lines
    (partition-by s/blank?)        ;; Group non-blank lines together and blank lines together
    (remove (comp s/blank? first)) ;; Remove blank lines
    (map (partial s/join " "))     ;; Join group of non-blank lines into a single line, aka a portfolio-str
    (map map<-portfolio-str)))     ;; Calculate portfolio

(defn valid-portfolio-v1? [ptf]
  (every? #(contains? ptf %) fields))


(spec/def ::byr #(when-let [n (parse-number %)] (<= 1920 n 2002)))
(spec/def ::iyr #(when-let [n (parse-number %)] (<= 2010 n 2020)))
(spec/def ::eyr #(when-let [n (parse-number %)] (<= 2020 n 2030)))
(spec/def ::hgt #(case (apply str (take-last 2 %))
                   "cm" (when-let [n (parse-number (apply str (drop-last 2 %)))] (<= 150 n 193))
                   "in" (when-let [n (parse-number (apply str (drop-last 2 %)))] (<= 59 n 76))
                   false))
(spec/def ::hcl (partial re-find #"^#[\da-fA-F]{6}$"))
(spec/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(spec/def ::pid (partial re-find #"^\d{9}$"))
(spec/def ::portfolio1 valid-portfolio-v1?)
(spec/def ::portfolio2 (spec/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]))

(defn result [spec-pred]
  (with-open [rdr (io/reader "./inputs/day4.txt")]
    (->> (line-seq rdr)
         parse-portfolios
         (filter (partial spec/valid? spec-pred))
         count)))

(println [(result ::portfolio1) (result ::portfolio2)])
