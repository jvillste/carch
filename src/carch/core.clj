(ns carch.core
  (:import [JavaPhotoArchive Archiver]
           [java.io File]
           [com.drew.imaging.jpeg JpegMetadataReader]
           [com.drew.metadata.exif ExifDirectory]))

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

