(ns advent-of-code.pull-input
  (:require [clojure.string :as str]
            [org.httpkit.client :as http.client]))

(def session-header
  (str/trim (slurp "resources/session.txt")))

(defn pull-solution
  [day]
  (let [url (format "https://adventofcode.com/2022/day/%d/input" day)
        opts {:headers {"Cookie"
	                      session-header}}
        {:keys [status headers body error] :as resp} @(http.client/get url
                                                                       opts)]
    (if error
      (println "Failed, exception: " error)
      (println "HTTP GET success: " status))
    body))

(def pull-solution-cached (memoize pull-solution))

(defn str->vector
  "Splits `s` into a vector separated be `delim` (which defaults to \"\n\")"
  ([s]
   (str->vector s "\n"))
  ([s delim]
   (str/split s (re-pattern delim))))
