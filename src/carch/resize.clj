(ns carch.resize
  (:require [carch.common :as common]
            [clojure.java.shell :as shell])
  (:import [java.io File]
           #_[java.nio.file Paths]
           [java.util Calendar Date]
           [com.drew.imaging.jpeg JpegMetadataReader]
           [com.drew.metadata.exif ExifSubIFDDirectory])
  (:use clojure.test))

(defn target-path [source-path source-dir target-dir]
  (let [relative-path (.substring source-path (.length source-dir))]
    (str target-dir relative-path)))

(defn resize-file [source-path target-path]
  (let [result (shell/sh "sips" "-s" "format" "jpeg" "-s" "formatOptions" "50" "-Z" "2000" source-path "--out" target-path)
        #_(shell/sh "/Users/jukka/bin/imagemagick/bin/convert" "-quality" "50" "-resize" "2000x2000" source-path target-path)]
    (when (not (= 0 (:exit result)))
      (println "Error when resizing:" (:err result))
      (throw (ex-info "Error when resizing" result)))
    result))

(defn resize [source-dir target-dir]
  (doseq [source-path (->> (common/files-in-directory source-dir)
                           (map #(.getAbsolutePath %))
                           (filter #(.endsWith % ".jpg"))
                           #_(filter (fn [path] (or (.contains path "2016")))))]
    (let [target-path (target-path source-path source-dir target-dir)]
      (if (.exists (File. target-path))
        (print ".")
        #_(println "already exists" target-path)
        (do (println source-path " -> " target-path)
            (.mkdirs (.getParentFile (File. target-path)))
            (resize-file source-path target-path))))))

(comment
  (resize-file "/Users/jukka/Pictures/uudet-kuvat/2022/2022-02-02/2022-02-02.16.21.26_810b11309cf8f407cdb9cc404813770b.CR3" "/Users/jukka/Downloads/test.jpg")
  ) ;; TODO: remove-me


#_(resize "/Volumes/BACKUP1/kuva-arkisto/" "/Users/jukka/Pictures/minikuva-arkisto/")

#_(resize "/Users/jukka/Downloads/uudet_kuvat/" "/Users/jukka/Downloads/arkisto_mini/")
