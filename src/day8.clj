(ns day8
  (:require  [clojure.java.io :as io]
             [clojure.string :as str]))

(defn parse-line [line]
  (let [[op vstr] (str/split line #" ")]
    [op (Integer. vstr)]))

(defn apply-code-to-state [[op v] [acc pos visited]]
  (case op
    "nop" [acc (inc pos) (conj visited pos)]
    "acc" [(+ acc v) (inc pos) (conj visited pos)]
    "jmp" [acc (+ pos v) (conj visited pos)]))

(defn apply-codes-to-state [codes [_ position visited :as state] f-if-visited]
  (cond
    (visited position) (f-if-visited state)
    (<= (count codes) position) state
    :else (recur codes
                 (apply-code-to-state (nth codes position) state)
                 f-if-visited)))

(defn swap-nth [codes position]
  (update-in codes [position 0] #({"nop" "jmp", "jmp" "nop"} % %)))

(defn transform-codes [codes]
  (let [indexed-codes (map-indexed vector codes) ;; Returns a vector of [n [op v]]
        swappable-codes (filter (comp #{"nop" "jmp"} first second) indexed-codes)
        swappable-positions (map first swappable-codes)]
    (map (partial swap-nth codes) swappable-positions)))

(with-open [rdr (io/reader "./inputs/day8.txt")]
  (let [codes (mapv parse-line (line-seq rdr))
        state [0 0 #{}] ;; [acc position visited]
        result1 (first (apply-codes-to-state codes state identity))
        codes-seq (transform-codes codes)
        result2 (->> codes-seq
                     (map #(apply-codes-to-state % state (constantly nil)))
                     (map first)
                     (drop-while not)
                     first)]
    (println [result1 result2])))
