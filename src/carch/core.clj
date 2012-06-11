(ns carch.core
  (:import [JavaPhotoArchive Archiver]
           [java.io File]
           [java.util Calendar]
           [com.drew.imaging.jpeg JpegMetadataReader]
           [com.drew.metadata.exif ExifDirectory])
  (:gen-class))

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

(defn files-in-directory [directory]
  (reduce (fn [files file] (if (.isDirectory file)
                             (concat files (files-in-directory file))
                             (conj files file)))
          []
          (.listFiles directory)))

(defn files-by-extension [directory extension]
  (filter #(.endsWith (.toLowerCase (.getName %)) (.toLowerCase extension))
          (files-in-directory directory)))

(defn photo-date [photo-file]
  (-> photo-file
      (JpegMetadataReader/readMetadata)
      (.getDirectory ExifDirectory)
      (.getDate ExifDirectory/TAG_DATETIME_DIGITIZED)))

(defn extension [file-name]
  (.substring file-name (+ 1 (.lastIndexOf file-name "."))))

(defn file-type [file]
  (case (.toLowerCase (extension (.getName file)))
    "jpg" :jpg
    "mov" :mov
    "avi" :avi))

(defn target-path-by-date [date]
  (let [calendar (Calendar/getInstance)]
    (.setTime calendar date)
    (str (.get calendar Calendar/YEAR)
         "//"
         (.get calendar Calendar/YEAR)
         "-"
         (format "%02d" (+ 1 (.get calendar Calendar/MONTH)))
         "-"
         (format "%02d" (.get calendar Calendar/DAY_OF_MONTH)))))

(defn target-path [file]
  (case (file-type file)
    :jpg (target-path-by-date (photo-date file))))

(defn -main [& args]
  (println "foo " (first args)))