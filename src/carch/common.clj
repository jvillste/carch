(ns carch.common
  (:import [java.io File])
  (:import [clojure.lang IReduceInit IReduce])
  (:require [clojure.test :refer :all]
            [carch.reduction :as reduction]))

(defn files-in-directory [directory-path]
  (reduce (fn [files file]
            (if (.isDirectory file)
              (concat files (files-in-directory (.getPath file)))
              (conj files file)))
          []
          (.listFiles (File. directory-path))))

(defn file-reducible [directory-path]
  (reduction/tree-reducible (File. directory-path)
                            (fn [file]
                              (when (.isDirectory file)
                                (sort-by #(.getName %)
                                         (.listFiles file))))))



(comment

  (reduction/do-reducible [path (reduction/educe (file-reducible "/tmp")
                                                 (take 2)
                                                 (map #(.getPath %)))]
                          (println path))

  (reduction/process! (file-reducible "/tmp")
                      (take 2)
                      (map #(.getPath %))
                      (map println))

  )
