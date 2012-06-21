(ns carch.main
  (:require [carch.core :as core])
  (:gen-class))

(defn -main [& args]
  (.start (Thread. (fn [] (apply core/start args))))
  (.start (Thread. core/command-line-ui))
  (read-line)
  (reset! core/running false))