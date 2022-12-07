(ns advent-of-code.day.five
  (:require [advent-of-code.pull-input :as pull-input]
            [clojure.math :as math]
            [clojure.string :as str]))

(def in (pull-input/str->vector (pull-input/pull-input-cached 5)))

(defn parse-element
  [s]
  (last (re-find #"\[([A-Z])\]" s)))

(defn parse-row
  "Returns char of current element of current stack for `s` or nil if none"
  [stacks s]
  (let [cols (map (partial apply str) (partition-all 4 s))
        elements (mapv parse-element cols)
        conj-some #(if %2 (conj %1 %2) %1)]
    (mapv conj-some stacks elements)))

(defn parse-initial-stacks
  [rows]
  (let [n-stacks (-> rows first count (/ 4) math/ceil long)
        stacks (into [] (repeat n-stacks []))]
    (reduce parse-row stacks (reverse rows))))

(defn parse-move
  [s]
  (let [[_ move _ from _ to] (str/split s #" ")
        [move from to] (mapv parse-long [move from to])
        [from to] (mapv dec [from to])]
    [move from to]))

(defn parse-moves
  [rows]
  (map parse-move rows))

(defn peek+pop
  "peek then pop from `v`, returning peeked"
  [[v popped]]
  (let [peeked (peek v)]
    [(pop v) (conj popped peeked)]))

(defn move-crates-9000
  [stacks [move from to]]
  (let [[popped-stack popped] (->> (get stacks from)
                                   (conj '([]))
                                   (iterate peek+pop)
                                   (take (inc move))
                                   last)]
    (-> stacks
        (assoc from popped-stack)
        (update to #(into % popped)))))

(defn partition-v
  [v partition-idx]
  [(subvec v 0 partition-idx)
   (subvec v partition-idx)])

(defn move-crates-9001
  [stacks [move from to]]
  (let [stack (get stacks from)
        [popped-stack popped] (partition-v stack (- (count stack) move))]
    (-> stacks
        (assoc from popped-stack)
        (update to #(into % popped)))))

(defn parse-input
  [s move-crate-f]
  (let [[stack-rows move-rows] (split-with (complement str/blank?) s)
        stacks (parse-initial-stacks stack-rows)
        moves (parse-moves (rest move-rows))]
    (reduce move-crate-f stacks moves)))

(defn solution-part1
  [input]
  (apply str (map peek (parse-input input move-crates-9000))))

(defn solution-part2
  [input]
  (apply str (map peek (parse-input input move-crates-9001))))
