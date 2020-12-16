(ns day16
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def data
  (str/split-lines (slurp "./inputs/day16.txt")))

(defn parse-numbers [comma-sep-nbs] (read-string (str \[ comma-sep-nbs \])))
(defn parse-rule [line]
  (let [[rule desc] (str/split line #": ")
        numbers (parse-numbers (str/replace desc #"(-| or )" ","))]
    [rule (partition 2 numbers)]))

(defn parse [data]
  (let [[rule-lines [_ ours-line] [_ & theirs-lines]]
        (->> data
             (partition-by str/blank?)
             (remove (comp str/blank? first)))]
    {:rules (into {} (mapv parse-rule rule-lines))
     :our (parse-numbers ours-line)
     :theirs (map parse-numbers theirs-lines)}))

(defn within-ranges? [n [[a b] [c d]]]
  (or (<= a n b) (<= c n d)))

(defn conforms-to [rules n]
  (->> rules
       (filter (comp (partial within-ranges? n) val))
       (map key)
       set))

(defn get-candidates [rules col]
  (->> col
       (map (partial conforms-to rules))
       (reduce set/intersection (set (keys rules)))))

(defn injest-candidate [so-far [possibilities position]]
  (let [resolved (keys so-far)
        next (first (apply disj possibilities resolved))]
    (assoc so-far next position)))

(defn rule-positions [rules tickets]
  (->> tickets
       (apply map vector)
       (map (partial get-candidates rules))
       (map-indexed (fn [i s] [s i]))
       (sort-by (comp count first))
       (reduce injest-candidate {})))

(defn part1 [data]
  (let [{:keys [rules theirs]} (parse data)
        get-invalid-numbers (partial remove (comp not-empty (partial conforms-to rules)))]
    (->> theirs
         (mapcat get-invalid-numbers)
         (reduce +))))

(defn part2 [data]
  (let [{:keys [rules our theirs]} (parse data)
        ticket-valid? (partial every? (comp not-empty (partial conforms-to rules)))]
    (->> theirs
         (filter ticket-valid?)
         (rule-positions rules)
         (filter #(-> % key (str/starts-with? "departure")))
         vals
         (map (partial nth our))
         (reduce *))))

(prn ((juxt part1 part2) data))
