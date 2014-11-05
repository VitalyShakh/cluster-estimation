(ns cluster-estimation.core-test
  (:require [clojure.test :refer :all]
            [cluster-estimation.core :refer :all]))

(deftest split-with-comma-test
  (is (= ["1" "2" "3" "4" "5"] (split-with-comma "1,2,3,4,5"))))

(deftest parse-parameters-test
  (is (= [1 2 3 4] (parse-parameters "1,2,3,4,5"))))

(deftest get-square-of-diff-test
  (are [v1 v2] (= 9.0 (get-square-of-diff v1 v2))
       0 3
       1 4
      -1 2))

(deftest get-distance-test
  (are [p1 p2] (= 5.0 (get-distance p1 p2))
       [1 0 0] [2 3 4]
       [3 20 24] [4 24 27]
       [5 -10 18] [6 -13 22]))

(deftest get-points-potential-test
  (is (= (Math/exp -12.5) (get-points-potential [1 0 0] [2 3 4] 0.5))))

(deftest get-potential-test
  (let [point [2 0 0]
        distance (Math/exp -12.5)
        expected-result (list (+ 1 distance distance) point)]
  (is (= expected-result (get-potential [[1 3 4] point [3 3 4]] point 0.5)))))

(deftest get-potentials-test
  (let [coef 0.5
        data [[1 0 0] [2 3 4] [3 3 4]]
        expected-result (list (get-potential data [1 0 0] coef)
                              (get-potential data [2 3 4] coef)
                              (get-potential data [3 3 4] coef))]
  (is (= expected-result (get-potentials [[1 0 0] [2 3 4] [3 3 4]] coef)))))

(deftest update-potentials-test
  (let [potentials [[14 [1 0 0]] [28 [2 3 4]]]
        core [28 [2 3 4]]
        beta 0.5
        exptected-result (list (vector (- 14 (* 28 (Math/exp -12.5))) [1 0 0])
                               (vector 0.0 [2 3 4]))]
    (is (= exptected-result (update-potentials potentials core beta)))))

(deftest reject-point-test
  (let [potentials [[23 [1 0 0]] [12 [2 1 1]] [22 [3 3 4]] [1 [4 12 27]] [5 [5 5 5]]]
       rejected-point [22 [3 3 4]]
       expected-result [[23 [1 0 0]] [12 [2 1 1]] [0 [3 3 4]] [1 [4 12 27]] [5 [5 5 5]]]]
    (is (= expected-result (reject-point potentials rejected-point)))))

(deftest get-min-distance-test
  (let [point [1 2 2]
        cores [[2 0 0] [3 0 4] [4 3 0]]
        expected-result (get-distance point [4 3 0])]
    (is (= expected-result (get-min-distance point cores)))))

(deftest get-cluster-cores-test
  (let [data [[1 3 0] [2 0 7] [3 10 0] [4 7 7] [5 9 -1]
              [6 2 7] [7 1 8] [8 1 6] [9 2 1] [10 3 2]
              [11 3 1] [12 6 6] [13 7 6] [14 7 5] [15 8 6]
              [16 8 0] [17 1 7] [18 9 1] [19 9 0] [20 4 1]]
        radius 5
        expected-result [[13 7 6] [11 3 1] [19 9 0] [17 1 7]]]
    (is (= expected-result (get-cluster-cores data radius)))))

(run-all-tests)
