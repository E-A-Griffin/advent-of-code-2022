(ns advent-of-code.day.six
  (:require [advent-of-code.pull-input :as pull-input]))

(def in (pull-input/pull-input-cached 6))

(defn distinct-n?
  [n coll]
  (= n (count (into #{} coll))))

(def distinct-4? (partial distinct-n? 4))
(def distinct-14? (partial distinct-n? 14))

(defn all-tuples
  "Takes `coll` and splits into all consecutive tuples of size `n`
  (including overlaps)"
  [n coll]
  (map (fn [i] [i (->> coll (drop i) (take n))]) (range (count coll))))

(defn solution
  [pred n input]
  (some (fn [[idx itm]] (when (pred itm) (+ n idx)))
        (all-tuples n input)))

(def solution-part1 (partial solution distinct-4? 4))
(def solution-part2 (partial solution distinct-14? 14))
