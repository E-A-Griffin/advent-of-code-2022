(ns advent-of-code.day.four
  (:require [advent-of-code.pull-input :as pull-input]
            [clojure.set :as set]))

(def in (pull-input/str->vector (pull-input/pull-input-cached 4)))

(defn parse-pair
  [s]
  (->> s
       (re-find #"([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)")
       rest
       (map parse-long)
       (partition 2)))

(defn contains-range?
  "True iff [`low1` `high1`] ⊆ [`low0` `high0`]"
  [[low0 high0] [low1 high1]]
  (and (>= low1 low0)
       (<= high1 high0)))

(defn contains-overlap?
  "True iff `low1`-`high1` ∩ `low0`-`high0` is NOT the empty set"
  [[[low0 high0] [low1 high1]]]
  (let [low-high->set (fn [low high] (set (range low (inc high))))
        r0 (low-high->set low0 high0)
        r1 (low-high->set low1 high1)
        not-empty? (complement empty?)]
    (not-empty? (set/intersection r0 r1))))

(defn redundant?
  "True iff `r0` is contained by `r1` or vice-versa"
  [[r0 r1]]
  (or (contains-range? r0 r1)
      (contains-range? r1 r0)))

(defn solution-part1
  [input]
  (count (filter redundant? (map parse-pair input))))

(defn solution-part2
  [input]
  (count (filter contains-overlap? (map parse-pair input))))
