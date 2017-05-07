(ns carch.thumbnails
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

(defn thumbnails [source-dir target-dir]
  (doseq [source-path (->> (common/files-in-directory source-dir)
                           (map #(.getAbsolutePath %))
                           (filter #(.endsWith % ".mp4"))
                           #_(filter (fn [path] (or (.contains path "2016")))))]
    (let [target-path (target-path source-path source-dir target-dir)]
      (if (.exists (File. target-path))
        (print ".")
        #_(println "already exists" target-path)
        (do (println source-path " -> " target-path)
            #_(.mkdirs (.getParentFile (File. target-path)))
            #_(shell/sh "sips" "-Z" "1024" "--setProperty" "formatOptions" "40" source-path "--out" target-path))))))

(thumbnails "/Volumes/BACKUP3/kuva-arkisto/2016" "/Users/jukka/Downloads/thumbnails")

#_(resize "/Users/jukka/Downloads/uudet_kuvat/" "/Users/jukka/Downloads/arkisto_mini/")
