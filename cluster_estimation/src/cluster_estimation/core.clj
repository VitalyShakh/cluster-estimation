(ns cluster-estimation.core
  (:require [clojure.string :as string])
  (:require [clojure.java.io :as io])
  (import java.lang.Math))

(defn split-with-comma [string] (string/split string #","))

(defn parse-parameters [input]
  (into [] (map read-string (drop-last (split-with-comma input)))))

(defn get-square-of-diff [v1 v2]
  (Math/pow (- v1, v2) 2))

(defn get-distance [p1 p2]
  (Math/sqrt(reduce + (map get-square-of-diff p1 p2))))


(defn read-file [file]
  ;(def data [])
  (with-open [rdr (io/reader file)]
    (doseq [line (line-seq rdr)]
      (println line)))
  )

;(get-distance [0 0] [3 4])

;(parse-parameters "1.12,21.22,123.0,123")

;(read-file "resources/glass.data.txt")
