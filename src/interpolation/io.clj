(ns interpolation.io
  (:require [clojure.string :as str])
  (:import [java.util Locale]))

(defn parse-line [line]
  (when-not (str/blank? line)
    (let [parts (str/split (str/trim line) #"\s+")]
      (when (= 2 (count parts))
        {:x (Double/parseDouble (first parts))
         :y (Double/parseDouble (second parts))}))))

(defn read-points
  "Returns a lazy sequence of points from stdin.
   Each point is a map {:x ... :y ...}"
  []
  (->> (line-seq (java.io.BufferedReader. *in*))
       (map parse-line)
       (filter some?)))

(defn format-result [algorithm-name x y]
  (String/format Locale/US "%s: %.4f %.4f"
                 (into-array Object [(name algorithm-name) (double x) (double y)])))

(defn print-result [algorithm-name x y]
  (println (format-result algorithm-name x y))
  (flush))

(defn print-results [algorithm-name points]
  (doseq [{:keys [x y]} points]
    (print-result algorithm-name x y)))
