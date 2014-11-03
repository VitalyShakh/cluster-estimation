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

(defn reject-point [potentials rejected-point]
  (map #(if (= rejected-point %) (list 0 (last rejected-point)) %) potentials))

(defn get-min-distance [point cores]
  (apply min (map #(get-distance point %) cores)))

(defn get-cluster-cores [data]
  (let [radius-a 5
        radius-b (* radius-a 1.5)
        alpha (/ 4 (Math/pow radius-a 2))
        beta (/ 4 (Math/pow radius-b 2))
        upper-threshold 0.5
        lower-threshold 0.15
        potentials (get-potentials data alpha)
        first-core (apply max-key first potentials)
        first-core-potential (first first-core)]

    (loop [potentials (update-potentials potentials first-core beta)
           cores (vector (last first-core))]
      (let [new-core (apply max-key first potentials)
            new-core-potential (first new-core)
            new-core-point (last new-core)]
      (cond
       (> new-core-potential (* upper-threshold first-core-potential))
          (recur (update-potentials potentials new-core beta) (conj cores new-core-point))

       (< new-core-potential (* lower-threshold first-core-potential)) cores

       (>= (+ (/ (get-min-distance new-core-point cores) radius-a) (/ new-core-potential first-core-potential)) 1)
         (recur (update-potentials potentials new-core beta) (conj cores new-core-point))

       :else
         (recur (reject-point potentials new-core) cores))))))

(defn read-file [file]
  (def data (atom []))
  (with-open [rdr (io/reader file)]
    (doseq [line (line-seq rdr)]
      (if (not= line "")
        (swap! data conj (parse-parameters line)))))
  (println (string/join "\n" (map str (get-cluster-cores @data)))))



;(read-file "resources/glass.data.txt")

;(get-cluster-cores [[1 3 0] [2 0 7] [3 10 0] [4 7 7] [5 9 -1] [6 2 7] [7 1 8] [8 1 6] [9 2 1] [10 3 2] [11 3 1] [12 6 6] [13 7 6] [14 7 5] [15 8 6] [16 8 0] [17 1 7] [18 9 1] [19 9 0] [20 4 1]])
