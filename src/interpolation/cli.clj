(ns interpolation.cli
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as str]))

(def cli-options
  [["-l" "--linear" "Use linear interpolation"]
   ["-N" "--newton" "Use Newton interpolation"]
   ["-L" "--lagrange" "Use Lagrange interpolation"]
   ["-s" "--step STEP" "Discretization step"
    :default 1.0
    :parse-fn #(Double/parseDouble %)]
   ["-n" "--points N" "Number of points for Newton/Lagrange"
    :default 4
    :parse-fn #(Integer/parseInt %)]
   ["-h" "--help" "Show help"]])

(defn usage [options-summary]
  (->> ["Streaming interpolation program"
        ""
        "Usage: interpolation [options]"
        ""
        "Options:"
        options-summary
        ""
        "Input format: x y (one point per line on stdin)"
        "Output format: algorithm: x y"]
       (str/join \newline)))

(defn error-msg [errors]
  (str "Errors:\n" (str/join \newline errors)))

(defn- get-algorithm
  "Extract single algorithm from options"
  [options]
  (cond
    (:linear options) :linear
    (:newton options) :newton
    (:lagrange options) :lagrange
    :else nil))

(defn validate-args [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)
        algorithm (get-algorithm options)]
    (cond
      (:help options)
      {:exit-message (usage summary) :ok? true}

      errors
      {:exit-message (error-msg errors) :ok? false}

      (nil? algorithm)
      {:exit-message "Error: Algorithm must be specified (--linear, --newton, or --lagrange)\n\n"
       :ok? false}

      :else
      {:options (assoc options :algorithm algorithm)})))
