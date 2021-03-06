(ns carch.main
  (:require [carch.core :as core])
  (:gen-class))

(defn copy [paths-string]
  (core/start (read-string paths-string)
              [(core/->PhotoArchiver) (core/->VideoArchiver)]))

(defn copy-photos [paths-string]
  (core/start (read-string paths-string)
              [(core/->PhotoArchiver)]))

(defn copy-with-config-file [paths-file-name]
  (copy (slurp paths-file-name)))

(defn resize [paths-string]
  (core/start (read-string paths-string)
              [(core/->ResizingPhotoArchiver)]))

(def commands [#'copy
               #'copy-photos
               #'copy-with-config-file
               #'resize])

(defn find-command [command-name commands]
  (first (filter (fn [command]
                   (= command-name
                      (name (:name (meta command)))))
                 commands)))

(defn -main [& command-line-arguments]
  (let [[command-name & arguments] command-line-arguments]
    (if-let [command (find-command command-name
                                   commands)]
      (apply command arguments)

      (do (println "Use one of the commands:")
          (println "------------------------")
          (println (->> commands
                        (map (fn [command-var]
                               (str (:name (meta command-var))
                                    ": "
                                    (:arglists (meta command-var))
                                    "\n"
                                    (:doc (meta command-var)))))
                        (interpose "------------------------\n")
                        (apply str)))))))
