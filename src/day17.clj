(ns day17
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]))

(def data
  (str/split-lines (slurp "./inputs/day17.txt")))

(defn to2D [data] (walk/postwalk vec data))
(defn to3D [data] (vector (to2D data)))
(defn to4D [data] (vector (to3D data)))
(defn shape [mat] (if-not (coll? mat) [] (concat [(count mat)] (shape (first mat)))))
(defn disabled [mat] (walk/postwalk #(if (coll? %) (vec %) \.) mat))

(defn wrap-vec [pre coll & [post]]
  (vec (concat (list pre) coll (list (or post pre)))))

(defn wrap [mat]
  (walk/postwalk #(cond->> % (coll? %) (wrap-vec (disabled (first %)))) mat))

(def around
  (let [d [-1 0 1]
        dzyx (for [z d, y d, x d :when (not= x y z 0)] [z y x])
        dwzyx (for [w d, z d, y d, x d :when (not= x y z w 0)] [w z y x])
        dpoint (comp {3 dzyx, 4 dwzyx} count)]
    (fn [point] (mapv (partial map + point) (dpoint point)))))

(defn coordinates
  ([mat] (apply coordinates (shape mat)))
  ([zn yn xn] (for [z (range zn), y (range yn), x (range xn)] [z y x]))
  ([wn zn yn xn] (for [w (range wn), [z y x] (coordinates zn yn xn)] [w z y x])))

(defn mutate-cell [mat pos]
  (let [active-around (count (filter (comp #{\#} #(get-in mat % \.)) (around pos)))
        cell (get-in mat pos)]
    (if (#{[\# 2] [\# 3] [\. 3]} [cell active-around]) \# \.)))

(defn mutate [mat]
(let [wrapped (wrap mat), mutated-cell (partial mutate-cell wrapped)]
  (reduce #(assoc-in %1 %2 (mutated-cell %2))
          (disabled wrapped)
          (coordinates wrapped))))

(defn calc [mat]
  (count (filter #{\#} (flatten (first (drop 6 (iterate mutate mat)))))))

(prn [(calc (to3D data)) (calc (to4D data))])
