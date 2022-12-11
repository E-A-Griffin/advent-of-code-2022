(ns advent-of-code.day.seven
  (:require [advent-of-code.pull-input :as pull-input]))

(def in (pull-input/str->vector (pull-input/pull-input-cached 7)))

(def simple-in (pull-input/str->vector "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
"))

(defn cd
  [[dir-tree path] args]
  (let [up? (= args "..")
        empty-dir {:type :dir
                   :size 0}
        dir (keyword args)]
    (if up?
      [dir-tree
       (subvec path 0 (dec (count path)))]
      [(if (not-empty dir-tree)
         (update-in dir-tree
                    path
                    #(conj %
                           {dir empty-dir}))
         (assoc empty-dir dir empty-dir))
       (conj path dir)])))

(defn ls
  [[dir-tree path] args]
  [(reduce (fn [tree file]
             (update-in tree path #(assoc %
                                          (-> file
                                              :name
                                              keyword)
                                          (dissoc file :name))))
           dir-tree
           args)
   path])

(defn parse-next
  [commands input]
  ;; Capital letters may or may not be valid, so just include them anyways
  (letfn [;; util
          (printerr [& args] (binding [*out* *err*] (apply println args)))
          ;; preds
          (regex-match? [re s] (boolean (re-matches re s)))
          (cd? [line] (regex-match? #"\$ cd [a-zA-Z\/\.]+" line))
          (ls? [line] (= line "$ ls"))
          (dir? [line] (regex-match? #"dir [a-zA-Z]+" line))
          (file? [line] (regex-match? #"[0-9]+ [a-zA-Z]+(\.[a-zA-Z]+)?" line))
          ;; parse functions
          (parse-cd [line]
             (->> line
                  (re-find #"\$ cd ([a-zA-Z\/\.]+)")
                  last))
          (parse-file [line] (cond
                               (dir? line) {:name
                                            (->> line
                                                 (re-find #"dir ([a-zA-Z]+)")
                                                 last)
                                            :size 0
                                            :type :dir}
                               (file? line) (let [[_ size name _]
                                                  (re-find
                                                   #"([0-9]+) ([a-zA-Z]+(\.[a-zA-Z]+)?)"
                                                   line)]
                                              {:name name
                                               :size (parse-long size)
                                               :type :file})
                               :else (printerr "Unparsable file:" line)))]
    (cond
      (cd? input) (conj commands {:f cd :args (parse-cd input)})
      (ls? input) (conj commands {:f ls :args []})
      ((some-fn dir? file?) input) (update-in commands
                                              [(-> commands count dec)
                                               :args]
                                              #(conj % (parse-file input)))
      :else (printerr "Syntax error:\n\t" input))))

(defn parse-input
  "Parses input into seq of [[cd]] and [[ls]] commands and arguments/results"
  [input]
  (reduce parse-next [] input))

(defn interpret-input
  [parsed-input]
  (reduce (fn [dir-tree+path {:keys [f args]}]
            (f dir-tree+path args))
          [{} []]
          parsed-input))

(defn calc-size
  [dir-tree limit]
  (let [file-size (:size dir-tree)]
    (if (= (:type dir-tree) :file)
      file-size
      (reduce (fn [acc [_k v]]
                (if (> acc limit)
                  (reduced acc)
                  (+ acc (calc-size v limit))))
              file-size
              (dissoc dir-tree :type :size)))))

(defn dir-tree->size-map
  [limit dir-tree]
  (reduce-kv (fn [m _k v]
               (if (= (:type v) :dir)
                 (into
                  (assoc m v (calc-size v limit))
                  (dir-tree->size-map limit v))
                 m))
             {}
             dir-tree))

(defn solution-part1
  [input]
  (let [dir-tree (-> input parse-input interpret-input first)
        limit 100000]
    (->> dir-tree
         (assoc {} :init)
         (dir-tree->size-map limit)
         vals
         (filter (partial > limit))
         (reduce +))))

(defn solution-part2
  [input]
  (let [total-diskspace      70000000
        needed-diskspace     30000000
        limit                Long/MAX_VALUE
        dir-tree             (-> input parse-input interpret-input first)
        size-map             (->> dir-tree
                                  (assoc {} :init)
                                  (dir-tree->size-map limit))
        [_ largest-dir-size] (->> size-map
                                  (sort-by val >)
                                  first)
        remaining            (- total-diskspace largest-dir-size)
        min-size             (- needed-diskspace remaining)]
    (->> size-map
         (filter (comp (partial <= min-size) val))
         vals
         (apply min))))
