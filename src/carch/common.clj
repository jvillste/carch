(ns carch.common
  (:require [carch.common :as common])
  (:import [java.io File]))

(defn files-in-directory [directory-path]
  (reduce (fn [files file] (if (.isDirectory file)
                             (concat files (files-in-directory (.getPath file)))
                             (conj files file)))
          []
          (.listFiles (File. directory-path))))
