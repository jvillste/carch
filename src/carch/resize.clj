(ns carch.resize
  (:require [carch.common :as common]
            [clojure.java.shell :as shell]
            [clojure.string :as string])
  (:import [java.io File]
           #_[java.nio.file Paths]
           [java.util Calendar Date]
           [com.drew.imaging.jpeg JpegMetadataReader]
           [com.drew.metadata.exif ExifSubIFDDirectory])
  (:use clojure.test))

(defn target-path [source-path source-dir target-dir]
  (let [relative-path (.substring source-path (.length source-dir))]
    (str target-dir relative-path)))

(defn image-dimensions [file-path]
  (let [[width height] (->> (string/split (:out (shell/sh "sips" "-g" "pixelWidth" "-g" "pixelHeight" file-path))
                                          #"\n")
                            (rest)
                            (map #(string/split % #" "))
                            (map last)
                            (map read-string))]
    {:width width
     :height height}))

(defn resize-file [source-path target-path]
  (let [dimensions (image-dimensions source-path)
        result (if (> 2000 (apply max (vals dimensions))) ;; -Z scales up images even if it should not
                 (shell/sh "sips" "-s" "format" "jpeg" "-s" "formatOptions" "50" source-path "--out" target-path)
                 (shell/sh "sips" "-s" "format" "jpeg" "-s" "formatOptions" "50" "-Z" "2000" source-path "--out" target-path))
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
  (resize-file "/Volumes/Backup_3_1/kuva-arkisto/2003/2003-06-15/2003-06-15.12.43.36_e464a05f9104f56a8950ee124f3dc6aa.jpg" "/Users/jukka/Downloads/test.jpg")
  (resize "/Volumes/BACKUP1/kuva-arkisto/" "/Users/jukka/Pictures/minikuva-arkisto/")
  (resize "/Users/jukka/Downloads/uudet_kuvat/" "/Users/jukka/Downloads/arkisto_mini/")
  )
