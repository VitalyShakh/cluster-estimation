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

(defn get-potential [data p alpha]
  (list (reduce + (map #(Math/exp (- (* alpha (Math/pow (get-distance % p) 2)))) data)) p))

(defn get-potentials [data alpha]
  (map #(get-potential data % alpha) data))

(defn get-cluster-cores [data]
  (last (apply max-key first (get-potentials data 0.5))))

(defn read-file [file]
  (def data (atom []))
  (with-open [rdr (io/reader file)]
    (doseq [line (line-seq rdr)]
      (if (not= line "")
        (swap! data conj (parse-parameters line)))))
  (println (get-cluster-cores @data)))


;(read-file "resources/glass.data.txt")
