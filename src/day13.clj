(ns day13
  (:require
   [clojure.test :as t]))

(def data (str/split-lines (slurp "./inputs/day13.txt")))

(def timestamp (Integer. (first data)))
(def bus-list (->> (str/split (second data) #",")
                   (map-indexed vector)
                   (remove (comp #{"x"} second))
                   (map #(update % 1 read-string))))

(defn part1 [time bus-ids]
  (let [bind-waiting-time (fn [bus] [(mod (- (mod time bus)) bus) bus])
        min-waiting (first (sort (mapv bind-waiting-time bus-ids)))]
    (reduce * min-waiting)))

(defn part2 [[[_ id1] & buses]]
  (loop [t id1, dt id1, remaining-buses buses]
    (if-not (seq remaining-buses)
      t
      (let [[[offset p] & rem] remaining-buses
            new-t (first (filter #(zero? (mod (+ % offset) p)) (iterate (partial + dt) t)))]
        (recur new-t (* dt p) rem)))))

(prn [(part1 timestamp (map second bus-list))
      (part2 bus-list)])
