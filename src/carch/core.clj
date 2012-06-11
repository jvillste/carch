(ns carch.core
  (:import [JavaPhotoArchive Archiver]
           [java.io File]))

(comment (Archiver/main (into-array String [])))

(defn operating-system []
  (case (System/getProperty "os.name")
    "Linux" :linux
    "Windows" :windows
    :unknown))

(defn roots []
  (case (operating-system)
    :linux (-> (File. "/media")
               (.listFiles))
    :windows (File/listRoots)))

(defn source-directory []
  (first (filter #(-> (File. (str (.getPath %) "/DCIM"))
                      (.exists))
                 (roots))))

(defn find-files [directory extension]
  (reduce (fn [files file] (if (.isDirectory file)
                             (concat files (find-files file extension))
                             (if (.endsWith (.toLowerCase (.getName file)) (.toLowerCase extension))
                               (conj files file)
                               files)))
          []
          (.listFiles directory)))

