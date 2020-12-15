(ns day15)

(defn nth-from-seed [numbers n]
  (loop [memory (into {} (map-indexed #(vector %2 (inc %1)) (drop-last 1 numbers)))
         latest (last numbers)
         step (count numbers)]
    (if (= n step)
      latest
      (recur (assoc memory latest step)
             (- step (get memory latest step))
             (inc step)))))

(def seed [0,1,4,13,15,12,16])
(prn (mapv (partial nth-from-seed seed) [2020 30000000]))
