(ns day7
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]))

(defn create-bag
  "Create a bag item given args"
  ([name]
   (create-bag name 1))
  ([name qty]
   (create-bag name qty []))
  ([name qty content]
   {:qty qty, :name name, :content content}))


(defn parse-line
  "Take a line as an input an returns a map"
  [line]
  (let [[bag content] (s/split line #" bags contain ")]
    (cond
      (= content "no other bags.") (create-bag bag)
      :else (create-bag bag 1 (into []
                                    (->> (re-seq #"(\d+) ((\w+\s?)+)(?= bag)" content)
                                         (map #(create-bag (nth % 2) (Integer. (second %))))))))))
;; PART 1

(defn contain-tree
  "Returns a simplified data structure"
  [[& bags]]
  (into {} (map #(hash-map (:name %) (set (map :name (:content %)))) bags)))


(defn which-of-contains
  "Returns the bags that contain directly at least one bag given its name"
  [tree bag-name]
  (let [sub-contain-trees (filter #(contains? (second %) bag-name) tree)]
    (set (map first sub-contain-trees)) )
  )

(defn which-of-contains-indirectly
  "Returns the bags that contain directly or indirectly a bag given its name"
  [tree bag-name]
  ;;containing : Return bags that contain at least a bag in a set
  (let [containing #(set (mapcat (partial which-of-contains tree) %))]
    (->> #{bag-name}
         (iterate containing)   ;; Recurs on each level
         (drop 1)               ;; Skip #{bag-name}, not relevant here
         (take-while not-empty) ;; Stop when no containing candidate
         (mapcat identity)      ;; Flat the sequence
         set)))                 ;; Dedupe

;; PART 2
(defn calc-bag-count
  "Returns the number of bags inside a bag"
  [bags bag-name]
  (let [get-bag #(first (filter (comp #{%} :name) bags))
        bag (get-bag bag-name)
        content (:content bag)]
    (if (empty? content)
      0
      (reduce + (map #(*
                       (:qty %)
                       (inc (calc-bag-count bags (:name %))))
                     content)))))


(with-open [rdr (io/reader "./inputs/day7.txt")]
  (let [bags (->> (line-seq rdr)
                  (map parse-line)
                  (into []))
        bags-by-direct-containers (contain-tree bags)
        indirect-containers (which-of-contains-indirectly
                               bags-by-direct-containers "shiny gold")
        result1 (count indirect-containers)
        result2 (calc-bag-count bags "shiny gold")
        result [result1 result2]]
    (println result)))
