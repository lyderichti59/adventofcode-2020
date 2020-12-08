(ns day7-test
  (:require [clojure.test :as t]
            [day7 :as sut]))

(t/deftest parse-line-test
  (let [line "light red bags contain 1 bright white bag, 2 muted yellow bags."]
    (t/is (= (sut/parse-line line) {:qty 1,
                                    :name "light red",
                                    :content [{:qty 1,
                                               :name "bright white"
                                               :content []}
                                              {:qty 2,
                                               :name "muted yellow"
                                               :content []}]}))))
