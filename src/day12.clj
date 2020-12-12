(ns day12
  (:require  [clojure.string :as str]))

(def data
  (str/split-lines (slurp "./inputs/day12.txt")))

(def clock-wise-rotations
  {0 identity
   90  (fn [[dx dy]] [dy (- dx)])
   180 (partial map -)
   270 (fn [[dx dy]] [(- dy) dx])})

(defn rotate [direction op v]
  ((get clock-wise-rotations (mod ({\R v, \L (- v)} op) 360)) direction))

(defn mutate-part1 [[east north :as pos] [dx dy :as dxy] op v]
  (case op
    nil [pos dxy]
    \N  [[east (+ north v)] dxy]
    \S  [[east (- north v)] dxy]
    \E  [[(+ east v) north] dxy]
    \W  [[(- east v) north] dxy]
    \F  [(mapv (fn [c dc a] (+ c (* dc a))) pos dxy [v v]) dxy]
    [pos (rotate dxy op v)]))

(defn mutate-part2 [[east north :as pos] [dx dy :as dxy] op v]
  (case op
    nil [pos dxy]
    \N  [pos [dx (+ dy v)]]
    \S  [pos [dx (- dy v)]]
    \E  [pos [(+ dx v) dy]]
    \W  [pos [(- dx v) dy]]
    \F  [(mapv (fn [c dc a] (+ c (* dc a))) pos dxy [v v]) dxy]
    [pos (rotate dxy op v)]))

(defn mature [mutate start dir codes]
  (let [finalpos
        (loop [pos start, dxy dir, instructions codes]
          (let [instruct (first instructions)
                op (first instruct)
                v (some->> instruct rest (apply str) Integer.)
                [new-pos new-dir] (mutate pos dxy op v)]
            (if-not op
              pos
              (recur new-pos new-dir (rest instructions)))))]
    (reduce + (map #(Math/abs %) finalpos))))

(println [(mature mutate-part1 [0 0] [ 1 0] data)
          (mature mutate-part2 [0 0] [10 1] data)])
