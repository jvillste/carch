(ns carch.fix-video-dates
  (:require [carch.core :as carch]
            [carch.exiftool :as exiftool]))

(defn fix-video-date [file-path]
  (exiftool/write-date-time file-path
                            (carch/date-from-file-name file-path)))

(defn fix-video-dates [path]
  (let [paths (map #(.getPath %)
                   (carch/files-for-archiver (carch/->VideoArchiver) path))
        path-count (count paths)]
    (doseq [[index path] (map-indexed vector paths)]
      (println index "/" path-count path)
      (fix-video-date path))))

(comment
  (fix-video-dates "/Volumes/LEENAN LEVY/pienet-kuvat"
                   #_"/Volumes/LEENAN LEVY/pienet-kuvat")
  ) ;; TODO: remove me
