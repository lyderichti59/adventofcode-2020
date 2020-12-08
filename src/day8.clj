(ns day8
  (:require  [clojure.java.io :as io]
             [clojure.string :as str]))

(defn parse-line
  "Parses a line"
  [line]
  (let [[op vstr] (str/split line #" ")]
    [op (Integer. vstr)]))

(defn calc-while-not-visited
  "Apply codes while the position as not already been visited"
  [codes [acc position visited] false-if-visited]
  (cond
    (and false-if-visited (visited position)) false
    (or (<= (count codes) position) (visited position)) acc
    :else (let [[op v] (nth codes position)
                refreshed-visited (conj visited position)]
            (case op
              "nop" (recur codes [acc (inc position) refreshed-visited] false-if-visited)
              "acc" (recur codes [(+ acc v) (inc position) refreshed-visited] false-if-visited)
              "jmp" (recur codes [acc (+ position v) refreshed-visited] false-if-visited)))))

(defn swap-nth
  "Swap 'jmp' and 'nop' at nth position"
  [codes position]
  (let [swap #(case % "nop" "jmp", "jmp" "nop", %)]
    (update-in codes [position 0] swap)))

(defn transform-codes
  "Return a lazy sequence of modified codes (swap of 'nop' and 'jmp' opcodes)"
  [codes]
  (let [indexed-codes (map-indexed vector codes) ;; Returns a vector of [n [op v]]
        swappable-codes (filter (comp #{"nop" "jmp"} first second) indexed-codes)]
    (map (comp (partial swap-nth codes) first) swappable-codes)))

(with-open [rdr (io/reader "./inputs/day8.txt")]
  (let [codes (->> (line-seq rdr)
                   (map parse-line)
                   vec)
        state [0 0 #{}] ;; [acc position visited]
        result1 (calc-while-not-visited codes state false)
        codes-seq (transform-codes codes)
        result2 (->> codes-seq
                     (map #(calc-while-not-visited % state true))
                     (drop-while not)
                     first)
        result [result1 result2]]
    (println result)))
