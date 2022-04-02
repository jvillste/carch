(ns carch.main
  (:require [carch.core :as core])
  (:gen-class))

(defn copy [paths-string]
  (core/start (read-string paths-string)
              [(core/->PhotoArchiver) (core/->VideoArchiver) (core/->XMPArchiver)]))

(defn copy-xmps [paths-string]
  (core/start (read-string paths-string)
              [(core/->XMPArchiver)]))

(defn copy-with-config-file [paths-file-name]
  (copy (slurp paths-file-name)))

(defn resize [paths-string]
  (core/start (read-string paths-string)
              [(core/->ResizingPhotoArchiver)]))

(defn resize-videos [paths-string]
  (core/start (read-string paths-string)
              [(core/->ResizingVideoArchiver)]))

(def commands [#'copy
               #'copy-with-config-file
               #'copy-xmps
               #'resize
               #'resize-videos])

(defn find-command [command-name commands]
  (first (filter (fn [command]
                   (= command-name
                      (name (:name (meta command)))))
                 commands)))

(defn -main [& command-line-arguments]
  (let [[command-name & arguments] command-line-arguments]
    (if-let [command (find-command command-name
                                   commands)]
      (do (apply command arguments)
          (System/exit 0))

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
