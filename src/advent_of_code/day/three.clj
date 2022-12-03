(ns advent-of-code.day.three
  (:require [advent-of-code.pull-input :as pull-input]
            [clojure.set :as set]
            [clojure.string :as str]))

(def in (pull-input/str->vector (pull-input/pull-input-cached 3)))

(defn upper?
  "True iff `c` is an uppercase character (assumes `c` is alphabetic)"
  [c]
  (= (str c) (str/upper-case c)))

(defn char->priority
  [c]
  (let [adjustment-f (if (upper? c)
                       #(-> % (- (int \A)) (+ 26))
                       #(- % (int \a)))]
    (-> c int adjustment-f inc)))

(defn dissect-str
  "Returns a 2 element vector of the first and last half of `s` represented as
  sets of chars"
  [s]
  (map set (split-at (/ (count s) 2) s)))

(defn common-char
  ([s]
   (let [[comp0 comp1] (dissect-str s)]
     (first (set/intersection comp0 comp1))))
  ([s0 s1 s2]
   (first (set/intersection (set s0) (set s1) (set s2)))))

(defn solution-part1
  [input]
  (reduce + (mapv (comp char->priority
                        common-char)
                  input)))

(defn solution-part2
  [input]
  (reduce + (mapv (comp char->priority
                        (partial apply common-char))
                  (partition 3 input))))
