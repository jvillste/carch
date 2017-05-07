(ns carch.main
  (:require [carch.core :as core])
  (:gen-class))

(defn -main [& args]
  (.start (Thread. (fn [] (core/start (read-string (slurp (first args)))))))
  (.start (Thread. core/command-line-ui))
  (read-line)
  (reset! core/running false))

#_ (-main "/Users/jukka/Dropbox/bin/kuvat_kortilta_macissa copy.clj")


