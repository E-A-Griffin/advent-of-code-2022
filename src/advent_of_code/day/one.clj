(ns advent-of-code.day.one
  (:require [advent-of-code.pull-input :as pull-input]
            [clojure.string :as str]))

(def in (pull-input/str->vector (pull-input/pull-input-cached 1)))

(defn get-calories
  [input]
  (map (comp (partial reduce +) (partial map parse-long))
       (remove (partial = '(""))
               (partition-by str/blank? input))))

(defn solution-part-1
  [input]
  (apply max (get-calories input)))

(defn solution-part-2
  [input]
  (reduce + (take-last 3 (sort (get-calories input)))))
