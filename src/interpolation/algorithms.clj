(ns interpolation.algorithms)

;; Linear interpolation
(defn linear-interpolate
  "Linear interpolation between two points.
   points - vector of 2 points [{:x x1 :y y1} {:x x2 :y y2}]
   x - the x value to interpolate"
  [points x]
  (let [{x1 :x y1 :y} (first points)
        {x2 :x y2 :y} (second points)]
    (if (= x1 x2)
      y1
      (+ y1 (* (- y2 y1) (/ (- x x1) (- x2 x1)))))))

;; Newton interpolation using divided differences
(defn- compute-divided-differences
  "Compute divided differences table.
   Returns a vector where element i is the i-th divided difference [y0, y1, ..., yi]"
  [points]
  (let [n (count points)
        xs (mapv :x points)
        ys (mapv :y points)]
    (loop [table [ys]
           k 1]
      (if (>= k n)
        (mapv first table)
        (let [prev (last table)
              next-col (vec (map-indexed
                             (fn [i _]
                               (/ (- (nth prev (inc i)) (nth prev i))
                                  (- (nth xs (+ i k)) (nth xs i))))
                             (range (- n k))))]
          (recur (conj table next-col) (inc k)))))))

(defn newton-interpolate
  "Newton interpolation using divided differences.
   points - vector of n points
   x - the x value to interpolate"
  [points x]
  (let [coeffs (compute-divided-differences points)
        xs (mapv :x points)
        n (count points)]
    (loop [result (first coeffs)
           term 1.0
           i 1]
      (if (>= i n)
        result
        (let [new-term (* term (- x (nth xs (dec i))))
              new-result (+ result (* (nth coeffs i) new-term))]
          (recur new-result new-term (inc i)))))))

;; Lagrange interpolation
(defn- lagrange-basis
  "Calculate Lagrange basis polynomial L_i(x)"
  [points i x]
  (let [xi (:x (nth points i))
        n (count points)]
    (reduce
     (fn [acc j]
       (if (= i j)
         acc
         (let [xj (:x (nth points j))]
           (* acc (/ (- x xj) (- xi xj))))))
     1.0
     (range n))))

(defn lagrange-interpolate
  "Lagrange interpolation.
   points - vector of n points
   x - the x value to interpolate"
  [points x]
  (let [n (count points)]
    (reduce
     (fn [sum i]
       (+ sum (* (:y (nth points i)) (lagrange-basis points i x))))
     0.0
     (range n))))

;; Unified interface
(defn interpolate
  "Interpolate using the specified algorithm.
   algorithm - :linear, :newton, or :lagrange
   points - vector of points
   x - the x value to interpolate"
  [algorithm points x]
  (case algorithm
    :linear (linear-interpolate points x)
    :newton (newton-interpolate points x)
    :lagrange (lagrange-interpolate points x)))

(defn required-points
  "Returns the minimum number of points required for an algorithm"
  [algorithm n]
  (case algorithm
    :linear 2
    :newton n
    :lagrange n))
