(ns carch.main
  (:require [carch.core :as core])
  (:gen-class))

(defn -main [& args]
  (println "foo2 " (first args)))