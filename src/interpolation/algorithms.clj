(ns interpolation.algorithms)

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

;;; Main interpolation function

(defmulti interpolate
  "Interpolate using the specified algorithm.
   algorithm - :linear, :newton, or :lagrange
   points - vector of points
   x - the x value to interpolate
   Returns the interpolated y value."
  (fn [algorithm points x] algorithm))

(defmethod interpolate :linear
  [_ points x]
  (let [{x1 :x y1 :y} (first points)
        {x2 :x y2 :y} (second points)]
    (if (= x1 x2)
      y1
      (+ y1 (* (- y2 y1) (/ (- x x1) (- x2 x1)))))))

(defmethod interpolate :newton
  [_ points x]
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

(defmethod interpolate :lagrange
  [_ points x]
  (let [n (count points)]
    (reduce
     (fn [sum i]
       (+ sum (* (:y (nth points i)) (lagrange-basis points i x))))
     0.0
     (range n))))

;;; Required points interface
(defmulti required-points
  "Returns the minimum number of points required for an algorithm"
  (fn [algorithm n] algorithm))

(defmethod required-points :linear
  [_ _]
  2)

(defmethod required-points :newton
  [_ n]
  n)

(defmethod required-points :lagrange
  [_ n]
  n)
