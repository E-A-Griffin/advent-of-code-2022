(ns advent-of-code.day.two
  (:require [advent-of-code.pull-input :as pull-input]
            [clojure.string :as str]))

(def rock     1)
(def paper    2)
(def scissors 3)

(def loss 0)
(def draw 3)
(def win  6)

(def rps-encoding-part1
  {:A rock     :X rock
   :B paper    :Y paper
   :C scissors :Z scissors})

(def rps-encoding-part2
  {:A rock     :X loss
   :B paper    :Y draw
   :C scissors :Z win})

(defn parse-pair
  [encoding-f pair]
  (map (comp encoding-f keyword) (str/split pair #" ")))

(def parse-pair-part1 (partial parse-pair rps-encoding-part1))
(def parse-pair-part2 (partial parse-pair rps-encoding-part2))

(def in (pull-input/str->vector (pull-input/pull-input-cached 2)))

(def wdl-m
  {rock     {rock     draw
             paper    win
             scissors loss}
   paper    {rock     loss
             paper    draw
             scissors win}
   scissors {rock     win
             paper    loss
             scissors draw}})

(defn invert-map
  "keys->vals and vals->keys"
  [m]
  (zipmap (vals m) (keys m)))

(defn optimal-choice-part2
  [[rps wdl]]
  [rps (-> rps wdl-m invert-map (get wdl))])

(defn win-draw-lose
  [rps0 rps1]
  (get-in wdl-m [rps0 rps1]))

(defn get-point-for-round
  [pair]
  (+ (second pair) (apply win-draw-lose pair)))

(defn solution-part1
  [input]
  (reduce + (mapv (comp get-point-for-round
                        parse-pair-part1)
                  input)))

(defn solution-part2
  [input]
  (reduce + (mapv (comp get-point-for-round
                        optimal-choice-part2
                        parse-pair-part2)
                  input)))
