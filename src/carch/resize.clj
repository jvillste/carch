(ns carch.core
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

(defn resize [source-dir target-dir]
  (doseq [source-path (->> (common/files-in-directory source-dir)
                           (map #(.getAbsolutePath %))
                           (filter #(.endsWith % ".jpg"))
                           (filter (fn [path] (or (.contains path "2015")))))]
    (let [target-path (target-path source-path source-dir target-dir)]
      (if (.exists (File. target-path))
        (print ".")
        #_(println "already exists" target-path)
        (do (println source-path " -> " target-path)
            (.mkdirs (.getParentFile (File. target-path)))
            (shell/sh "sips" "-Z" "1024" "--setProperty" "formatOptions" "40" source-path "--out" target-path))))))

(resize "/Volumes/BACKUP1/kuva-arkisto/" "/Users/jukka/Pictures/arkisto_mini/")

#_(resize "/Users/jukka/Downloads/uudet_kuvat/" "/Users/jukka/Downloads/arkisto_mini/")
