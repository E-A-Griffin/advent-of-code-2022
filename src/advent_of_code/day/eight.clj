(ns advent-of-code.day.eight
  (:require [advent-of-code.pull-input :as pull-input]))

(def in (->> 8
             pull-input/pull-input-cached
             pull-input/str->vector
             (mapv (partial mapv (comp parse-long str)))))

(def simple-in
  [[3 0 3 7 3]
   [2 5 5 1 2]
   [6 5 3 3 2]
   [3 3 5 4 9]
   [3 5 3 9 0]])

(defn remove+split-at
  "Remove element at `idx` of `v`, splitting into [before-idx after-idx]"
  [v idx]
  (cond
    (zero? idx) [[] (subvec v 1)]
    (= idx (dec (count v))) [(subvec v 0 (dec (count v))) []]
    :else [(subvec v 0 idx)
           (subvec v (inc idx))]))

(defn row+col+tree
  [matrix row-idx col-idx]
  (let [row (get matrix row-idx)
        col (mapv #(get % col-idx) matrix)
        tree (get-in matrix [row-idx col-idx])]
    [row col tree]))

(defn edge?
  "True iff (`row-idx` `col-idx`) is on the permiter of `matrix`"
  [matrix row-idx col-idx]
  (or (zero? row-idx)
      (zero? col-idx)
      (= row-idx (-> matrix count dec))
      (= col-idx (-> matrix first count dec))))

(defn visible?
  [matrix row-idx col-idx]
  (or (edge? matrix row-idx col-idx)
      (let [[row col tree] (row+col+tree matrix row-idx col-idx)
            max-neighbors (fn [v idx]
                            (mapv (partial reduce max 0)
                                  (remove+split-at v idx)))
            [tall-left tall-right] (max-neighbors row col-idx)
            [tall-above tall-below] (max-neighbors col row-idx)]
        (not= tree (min tree tall-left tall-right tall-above tall-below)))))

(defn viewing-distance
  [tree neighbors]
  (let [[shorter [first-tall & _]] (split-with (partial > tree) neighbors)]
    (->> (conj shorter first-tall)
         (filter some?)
         count)))

(defn scenic-score
  [matrix row-idx col-idx]
  (if (edge? matrix row-idx col-idx)
    0
    (let [[row col tree] (row+col+tree matrix row-idx col-idx)
          [left-backwards right] (remove+split-at row col-idx)
          [above-backwards below] (remove+split-at col row-idx)
          above (rseq above-backwards)
          left (rseq left-backwards)]
    (reduce *
            (map (partial viewing-distance tree)
                 [left right above below])))))

(defn indexed-matrix-reduction
  "Apply `f` to `matrix` for every (row-idx col-idx)"
  [f matrix]
  (->> matrix
       (map-indexed (fn [row-idx row]
                      (map-indexed (fn [col-idx _]
                                     (f matrix row-idx col-idx))
                                   row)))))

(defn solution-part1
  [input]
  (->> input
       (indexed-matrix-reduction visible?)
       flatten
       (filter true?)
       count))

(defn solution-part2
  [input]
  (->> input
       (indexed-matrix-reduction scenic-score)
       flatten
       (reduce max)))
