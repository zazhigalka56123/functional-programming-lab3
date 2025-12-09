(ns interpolation.streaming
  (:require [interpolation.algorithms :as alg]
            [interpolation.io :as io]))

(defn generate-x-values
  "Generate x values from start to end with given step"
  [start end step]
  (take-while #(<= % (+ end 1e-9))
              (iterate #(+ % step) start)))

(defn- process-first-window
  "Process the first window - calculate from start to center"
  [algorithm window step]
  (let [x-start (:x (first window))
        x-end (:x (nth window (quot (count window) 2)))]
    (for [x (generate-x-values x-start x-end step)]
      {:x x :y (alg/interpolate algorithm window x)})))

(defn- process-middle-window
  "Process a middle window - calculate only the center point"
  [algorithm window step last-x]
  (let [center-idx (quot (count window) 2)
        x-center (:x (nth window center-idx))
        x-start (+ last-x step)]
    (for [x (generate-x-values x-start x-center step)]
      {:x x :y (alg/interpolate algorithm window x)})))

(defn- process-last-window
  "Process the last window - calculate from center to end"
  [algorithm window step last-x]
  (let [x-end (:x (last window))
        x-start (+ last-x step)]
    (for [x (generate-x-values x-start x-end step)]
      {:x x :y (alg/interpolate algorithm window x)})))

(defn- process-window-linear
  "Process a window for linear interpolation (special case - 2 points)"
  [window step last-x is-first?]
  (let [x-start (if is-first? (:x (first window)) (+ last-x step))
        x-end (:x (second window))]
    (for [x (generate-x-values x-start x-end step)]
      {:x x :y (alg/linear-interpolate window x)})))

(defn create-processor
  "Create a streaming processor for the given algorithm"
  [algorithm window-size step]
  (let [state (atom {:buffer []
                     :last-x nil
                     :is-first? true})]
    (fn [point]
      (let [{:keys [buffer last-x is-first?]} @state
            new-buffer (conj buffer point)]
        (if (< (count new-buffer) window-size)
          (do
            (swap! state assoc :buffer new-buffer)
            [])
          (let [window (vec (take-last window-size new-buffer))
                results (if (= algorithm :linear)
                          (process-window-linear window step last-x is-first?)
                          (if is-first?
                            (process-first-window algorithm window step)
                            (process-middle-window algorithm window step last-x)))
                new-last-x (if (seq results) (:x (last results)) last-x)]
            (swap! state assoc
                   :buffer new-buffer
                   :last-x new-last-x
                   :is-first? false)
            results))))))

(defn finalize-processor
  "Finalize processing when EOF is reached"
  [algorithm window-size step state-atom]
  (let [{:keys [buffer last-x is-first?]} @state-atom]
    (when (>= (count buffer) window-size)
      (let [window (vec (take-last window-size buffer))]
        (if (= algorithm :linear)
          []
          (process-last-window algorithm window step (or last-x (:x (first window)))))))))

(defn stream-interpolate
  "Stream interpolation for a single algorithm.
   Processes points as they arrive and outputs results immediately."
  [algorithm points step n]
  (let [window-size (alg/required-points algorithm n)
        state (atom {:buffer []
                     :last-x nil
                     :is-first? true
                     :output-started? false})]
    (doseq [point points]
      (let [{:keys [buffer last-x is-first? output-started?]} @state
            new-buffer (conj buffer point)]
        (if (< (count new-buffer) window-size)
          (swap! state assoc :buffer new-buffer)
          (let [window (vec (take-last window-size new-buffer))
                results (cond
                          (= algorithm :linear)
                          (process-window-linear window step last-x is-first?)

                          is-first?
                          (process-first-window algorithm window step)

                          :else
                          (process-middle-window algorithm window step last-x))
                new-last-x (if (seq results) (:x (last results)) last-x)]
            (doseq [r results]
              (io/print-result algorithm (:x r) (:y r)))
            (swap! state assoc
                   :buffer new-buffer
                   :last-x new-last-x
                   :is-first? false
                   :output-started? true)))))
    ;; Process final window for non-linear algorithms
    (when (and (not= algorithm :linear)
               (>= (count (:buffer @state)) window-size))
      (let [{:keys [buffer last-x]} @state
            window (vec (take-last window-size buffer))
            results (process-last-window algorithm window step
                                         (or last-x (:x (first window))))]
        (doseq [r results]
          (io/print-result algorithm (:x r) (:y r)))))))

(defn stream-interpolate-multi
  "Stream interpolation for multiple algorithms simultaneously"
  [algorithms points step n]
  (let [points-vec (vec points)]
    (doseq [algo algorithms]
      (stream-interpolate algo points-vec step n))))

