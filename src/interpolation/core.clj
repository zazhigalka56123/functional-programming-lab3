(ns interpolation.core
  (:require [interpolation.cli :as cli]
            [interpolation.io :as io]
            [interpolation.streaming :as streaming])
  (:gen-class))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn run [options]
  (let [algorithm (:algorithm options)
        step (:step options)
        n (:points options)
        points (io/read-points)]
    (streaming/stream-interpolate algorithm points step n)))

(defn -main [& args]
  (let [{:keys [options exit-message ok?]} (cli/validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (run options))))
