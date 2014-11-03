(ns cluster-estimation.core
  (:require [clojure.string :as string])
  (:require [clojure.java.io :as io])
  (import java.lang.Math))

(defn split-with-comma [string] (string/split string #","))

(defn parse-parameters [input]
  (into [] (map read-string (drop-last (split-with-comma input)))))

(defn get-square-of-diff [v1 v2]
  (Math/pow (- v1, v2) 2))

(defn get-distance [p1 p2] ;drop first element because first parameter is object id!
  (Math/sqrt(reduce + (drop 1 (map get-square-of-diff p1 p2)))))

;gets potetial between two points
(defn get-points-potential [p1 p2 coef]
  (Math/exp (- (* coef (Math/pow (get-distance p1 p2) 2)))))

(defn get-potential [data p alpha]
  (list (reduce + (map #(get-points-potential % p alpha) data)) p))

(defn get-potentials [data alpha]
  (map #(get-potential data % alpha) data))

(defn update-potentials [potentials core beta]
  (map #(vector (- (first %) (* (first core) (get-points-potential (last %) (last core) beta))) (last %)) potentials))

(defn get-cluster-cores [data]
  (let [radius-a 5
        radius-b (* radius-a 1.5)
        alpha (/ 4 (Math/pow radius-a 2))
        beta (/ 4 (Math/pow radius-b 2))
        upper-threshold 0.5
        lower-threshold 0.15
        potentials (get-potentials data alpha)
        first-core (apply max-key first potentials)
        cores (atom[(last first-core)])]
    ;(loop [potentials (update-potentials(potentials first-core beta))
           ;new-core (apply max-key first potentials)]
        ;(swap! cores conj (last new-core))
        ;()
        @cores;)
  ))

(defn read-file [file]
  (def data (atom []))
  (with-open [rdr (io/reader file)]
    (doseq [line (line-seq rdr)]
      (if (not= line "")
        (swap! data conj (parse-parameters line)))))
  (println (string/join "\n" (map str (get-cluster-cores @data)))))

;(read-file "resources/glass.data.txt")

;(update-potentials [[14 [1 0 0]] [28 [2 3 4]]] [28 [1 3 4]] 0.07111)
