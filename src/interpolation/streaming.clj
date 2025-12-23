(ns interpolation.streaming
  (:require [interpolation.algorithms :as alg]
            [interpolation.io :as io]))

(defn generate-x-values
  "Generate x values from start to end with given step"
  [start end step]
  (take-while #(<= % (+ end 1e-9))
              (iterate #(+ % step) start)))

;;; State management
(defn init-state
  "Initialize state for streaming interpolation"
  [algorithm window-size step]
  {:algorithm algorithm
   :window-size window-size
   :step step
   :buffer []
   :last-x nil})

(defn- get-window [state]
  (vec (take-last (:window-size state) (:buffer state))))

(defn- has-full-window? [state]
  (>= (count (:buffer state)) (:window-size state)))

;;; Core interpolation logic
(defn- interpolate-range
  "Generate interpolated points from x-start to x-end.
   Returns [updated-state results]"
  [state x-start x-end]
  (let [{:keys [algorithm step]} state
        window (get-window state)
        results (for [x (generate-x-values x-start x-end step)]
                  {:x x :y (alg/interpolate algorithm window x)})
        new-last-x (if (seq results) (:x (last results)) (:last-x state))]
    [(assoc state :last-x new-last-x) results]))

;;; X-end calculation strategies
(defn- x-end-for-linear [window]
  (:x (second window)))

(defn- x-end-for-polynomial [window]
  (:x (nth window (quot (count window) 2))))

(defn- x-end-for-finalize [window]
  (:x (last window)))

;;; Main processing function
(defn process-point
  "Process a single point: add to buffer and interpolate if possible.
   Returns [new-state results]"
  [state point]
  (let [state (update state :buffer conj point)]
    (if-not (has-full-window? state)
      [state []]
      (let [window (get-window state)
            {:keys [algorithm last-x step]} state
            first-window? (nil? last-x)
            x-start (if first-window? 
                      (:x (first window)) 
                      (+ last-x step))
            x-end (if (= algorithm :linear)
                    (x-end-for-linear window)
                    (x-end-for-polynomial window))]
        (interpolate-range state x-start x-end)))))

;;; Finalization
(defn finalize-stream
  "Process the final window when stream ends"
  [state]
  (when (and (has-full-window? state) 
             (not= (:algorithm state) :linear)
             (:last-x state))
    (let [window (get-window state)
          x-start (+ (:last-x state) (:step state))
          x-end (x-end-for-finalize window)]
      (second (interpolate-range state x-start x-end)))))

;;; Public API
(defn stream-interpolate
  "Stream interpolation for a single algorithm.
   Processes points as they arrive and outputs results immediately."
  [algorithm points step n]
  (let [window-size (alg/required-points algorithm n)
        initial-state (init-state algorithm window-size step)]
    (loop [state initial-state
           remaining-points points]
      (if-let [point (first remaining-points)]
        (let [[new-state results] (process-point state point)]
          (doseq [r results]
            (io/print-result algorithm (:x r) (:y r)))
          (recur new-state (rest remaining-points)))
        (when-let [final-results (finalize-stream state)]
          (doseq [r final-results]
            (io/print-result algorithm (:x r) (:y r))))))))
