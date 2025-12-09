(ns interpolation.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [interpolation.algorithms :as alg]
            [interpolation.cli :as cli]
            [interpolation.io :as io]))

(def epsilon 1e-6)

(defn approx= [a b]
  (< (Math/abs (- a b)) epsilon))

;; Linear interpolation tests
(deftest linear-interpolation-test
  (testing "Linear interpolation between two points"
    (let [points [{:x 0 :y 0} {:x 1 :y 1}]]
      (is (approx= 0.0 (alg/linear-interpolate points 0)))
      (is (approx= 0.5 (alg/linear-interpolate points 0.5)))
      (is (approx= 1.0 (alg/linear-interpolate points 1)))))

  (testing "Linear interpolation with different slope"
    (let [points [{:x 0 :y 0} {:x 2 :y 4}]]
      (is (approx= 0.0 (alg/linear-interpolate points 0)))
      (is (approx= 2.0 (alg/linear-interpolate points 1)))
      (is (approx= 4.0 (alg/linear-interpolate points 2)))))

  (testing "Linear interpolation with negative values"
    (let [points [{:x -1 :y -2} {:x 1 :y 2}]]
      (is (approx= 0.0 (alg/linear-interpolate points 0)))
      (is (approx= -2.0 (alg/linear-interpolate points -1)))
      (is (approx= 2.0 (alg/linear-interpolate points 1))))))

;; Newton interpolation tests
(deftest newton-interpolation-test
  (testing "Newton interpolation for linear function"
    (let [points [{:x 0 :y 0} {:x 1 :y 1} {:x 2 :y 2}]]
      (is (approx= 0.0 (alg/newton-interpolate points 0)))
      (is (approx= 0.5 (alg/newton-interpolate points 0.5)))
      (is (approx= 1.5 (alg/newton-interpolate points 1.5)))))

  (testing "Newton interpolation for quadratic function (y = x^2)"
    (let [points [{:x 0 :y 0} {:x 1 :y 1} {:x 2 :y 4}]]
      (is (approx= 0.0 (alg/newton-interpolate points 0)))
      (is (approx= 1.0 (alg/newton-interpolate points 1)))
      (is (approx= 4.0 (alg/newton-interpolate points 2)))
      (is (approx= 0.25 (alg/newton-interpolate points 0.5)))))

  (testing "Newton interpolation with 4 points"
    (let [points [{:x 0 :y 0} {:x 1 :y 1} {:x 2 :y 8} {:x 3 :y 27}]]
      (is (approx= 0.0 (alg/newton-interpolate points 0)))
      (is (approx= 1.0 (alg/newton-interpolate points 1)))
      (is (approx= 8.0 (alg/newton-interpolate points 2)))
      (is (approx= 27.0 (alg/newton-interpolate points 3))))))

;; Lagrange interpolation tests
(deftest lagrange-interpolation-test
  (testing "Lagrange interpolation for linear function"
    (let [points [{:x 0 :y 0} {:x 1 :y 1} {:x 2 :y 2}]]
      (is (approx= 0.0 (alg/lagrange-interpolate points 0)))
      (is (approx= 0.5 (alg/lagrange-interpolate points 0.5)))
      (is (approx= 1.5 (alg/lagrange-interpolate points 1.5)))))

  (testing "Lagrange interpolation for quadratic function (y = x^2)"
    (let [points [{:x 0 :y 0} {:x 1 :y 1} {:x 2 :y 4}]]
      (is (approx= 0.0 (alg/lagrange-interpolate points 0)))
      (is (approx= 1.0 (alg/lagrange-interpolate points 1)))
      (is (approx= 4.0 (alg/lagrange-interpolate points 2)))
      (is (approx= 0.25 (alg/lagrange-interpolate points 0.5)))))

  (testing "Lagrange interpolation matches Newton for same points"
    (let [points [{:x 0 :y 1} {:x 1 :y 3} {:x 2 :y 7} {:x 3 :y 13}]]
      (doseq [x [0.5 1.5 2.5]]
        (is (approx= (alg/newton-interpolate points x)
                     (alg/lagrange-interpolate points x)))))))

;; CLI tests
(deftest cli-test
  (testing "Valid arguments parsing"
    (let [result (cli/validate-args ["--linear" "--step" "0.5"])]
      (is (nil? (:exit-message result)))
      (is (:linear (:options result)))
      (is (= 0.5 (:step (:options result))))))

  (testing "Multiple algorithms"
    (let [result (cli/validate-args ["--linear" "--newton" "-n" "4"])]
      (is (nil? (:exit-message result)))
      (is (:linear (:options result)))
      (is (:newton (:options result)))
      (is (= 4 (:points (:options result))))))

  (testing "No algorithm specified"
    (let [result (cli/validate-args ["--step" "0.5"])]
      (is (some? (:exit-message result)))
      (is (not (:ok? result)))))

  (testing "Get algorithms list"
    (is (= [:linear] (cli/get-algorithms {:linear true})))
    (is (= [:linear :newton] (cli/get-algorithms {:linear true :newton true})))
    (is (= [:linear :newton :lagrange]
           (cli/get-algorithms {:linear true :newton true :lagrange true})))))

;; IO tests
(deftest io-test
  (testing "Parse valid line"
    (is (= {:x 1.0 :y 2.0} (io/parse-line "1 2")))
    (is (= {:x 1.5 :y 2.5} (io/parse-line "1.5 2.5")))
    (is (= {:x -1.0 :y -2.0} (io/parse-line "-1 -2"))))

  (testing "Parse line with tabs"
    (is (= {:x 1.0 :y 2.0} (io/parse-line "1\t2"))))

  (testing "Parse invalid lines"
    (is (nil? (io/parse-line "")))
    (is (nil? (io/parse-line "   ")))
    (is (nil? (io/parse-line "1"))))

  (testing "Format result"
    (is (= "linear: 1.0000 2.0000" (io/format-result :linear 1 2)))
    (is (= "newton: 0.5000 0.2500" (io/format-result :newton 0.5 0.25)))))

;; Unified interpolate function tests
(deftest interpolate-test
  (testing "Unified interpolate function"
    (let [points [{:x 0 :y 0} {:x 1 :y 1}]]
      (is (approx= 0.5 (alg/interpolate :linear points 0.5))))
    (let [points [{:x 0 :y 0} {:x 1 :y 1} {:x 2 :y 4}]]
      (is (approx= 0.25 (alg/interpolate :newton points 0.5)))
      (is (approx= 0.25 (alg/interpolate :lagrange points 0.5))))))

(deftest required-points-test
  (testing "Required points for each algorithm"
    (is (= 2 (alg/required-points :linear 4)))
    (is (= 4 (alg/required-points :newton 4)))
    (is (= 4 (alg/required-points :lagrange 4)))
    (is (= 6 (alg/required-points :newton 6)))))

