(ns carch.main
  (:require [carch.core :as core])
  (:gen-class))

(defn -main [& args]
  (core/start (read-string (slurp (first args)))))
