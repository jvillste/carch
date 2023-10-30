(ns carch.resize
  (:require [carch.common :as common]
            [clojure.java.shell :as shell]
            [clojure.string :as string]
            [babashka.fs :as fs]
            [jx.java.shell :as jx-java-shell])
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

(defn size-in-megabytes [file-name]
  (/ (fs/size file-name)
     1000000))

(defn resize-video [source-path target-path]
  (let [result (jx-java-shell/timeout-sh  (size-in-megabytes source-path)
                                          "ffmpeg"
                                          "-i" source-path
                                          "-c:v" "libx265"
                                          "-crf" "28"
                                          "-preset" "fast"
                                          "-c:a" "aac"
                                          "-b:a" "128k"
                                          "-tag:v" "hvc1"
                                          "-vf" "scale='min(800,iw)':-2"
                                          target-path)]
    (when (not (= 0 (:exit result)))
      (println "Error when resizing:" (:err result))
      (throw (ex-info "Error when resizing" result)))
    result))

(defn target-to-source-file-name [target-file-name]
  (-> target-file-name
      (string/replace "/Users/jukka/Pictures/pienet-kuvat/"
                      "/Volumes/Backup_3_2/kuva-arkisto/")
      (string/replace "-small.mp4" "")))

(deftest test-source-to-target-file-name
  (is (= "/Volumes/Backup_3_2/kuva-arkisto/2017/2017-12-27/2017-12-27.17.40.03_5ff6fbc11fd2e8dbc43b4daf772ffc0c.MOV"
         (target-to-source-file-name "/Users/jukka/Pictures/pienet-kuvat/2017/2017-12-27/2017-12-27.17.40.03_5ff6fbc11fd2e8dbc43b4daf772ffc0c.MOV-small.mp4"))))

(defn target-path-to-glob-parameters [target-path]
  [(str "/Volumes/Backup_3_2/kuva-arkisto/" (subs target-path 35 51))
   (str "**" (subs target-path 71 103) ".*")])

(deftest test-target-path-to-glob-parameters
  (is (= ["/Volumes/Backup_3_2/kuva-arkisto/2018/2018-02-23/"
          "**4ca93a0f33a722da054a881c011e590d.*"]
         (target-path-to-glob-parameters "/Users/jukka/Pictures/pienet-kuvat/2018/2018-02-23/2018-02-23.18.48.16_4ca93a0f33a722da054a881c011e590d.MOV-small.mp4"))))

(defn target-to-fuzzy-source-path [target-path]
  (first (apply fs/glob (target-path-to-glob-parameters target-path))))

(defn timeout [timeout-ms callback]
  (let [fut (future (callback))
        ret (deref fut timeout-ms ::timed-out)]
    (when (= ret ::timed-out)
      (future-cancel fut))
    ret))


(comment
  (subs "/Volumes/Backup_3_2/kuva-arkisto/2017/2017-12-27/2017-12-27.17.40.03_5ff6fbc11fd2e8dbc43b4daf772ffc0c.MOV"
        69 101) ;; => "5ff6fbc11fd2e8dbc43b4daf772ffc0c"

  (subs "/Volumes/Backup_3_2/kuva-arkisto/2017/2017-12-27/2017-12-27.17.40.03_5ff6fbc11fd2e8dbc43b4daf772ffc0c.MOV"
        33 49) ;; => "2017/2017-12-27/"
  (resize-video "/Users/jukka/Downloads/IMG_7224.MOV"
                "/Users/jukka/Downloads/IMG_7224-small.MOV")

  (float (/ (fs/size "/Users/jukka/Pictures/uudet-lajittelemattomat-kuvat/DCIM/ZoomerangVideos/zoomerang_1674239084640.mp4")
            1000000))

  (do
    (fs/delete "/Users/jukka/Downloads/small.mp4")
    (resize-video "/Users/jukka/Pictures/uudet-lajittelemattomat-kuvat/DCIM/ZoomerangVideos/zoomerang_1674239084640.mp4"
                  "/Users/jukka/Downloads/small.mp4"))

  (time (do (fs/delete "/Users/jukka/Downloads/small.mp4")
            (resize-video "/Users/jukka/Pictures/uudet-lajittelemattomat-kuvat/DCIM/ZoomerangVideos/zoomerang_1674281404923.mp4"
                          "/Users/jukka/Downloads/small.mp4")))

  (resize-video "/Volumes/Backup_3_2/kuva-arkisto/2017/2017-12-27/2017-12-27.17.40.03_5ff6fbc11fd2e8dbc43b4daf772ffc0c.MOV"
                (target-to-source-file-name (str (first small)))
                (str (first small)))
  (fs/delete "/Users/jukka/Downloads/2017-12-27.17.40.03_5ff6fbc11fd2e8dbc43b4daf772ffc0c-small.mp4")


  (fs/size "/Users/jukka/Pictures/pienet-kuvat/2017/2017-05-09/2017-05-09.16.52.32_01df26fef0a0e569dd08d5ac33ffabc1.mov-small.mp4")
  (fs/size (target-to-source-file-name "/Users/jukka/Pictures/pienet-kuvat/2017/2017-05-09/2017-05-09.16.52.32_01df26fef0a0e569dd08d5ac33ffabc1.mov-small.mp4"))
  (def small (->> (fs/glob "/Users/jukka/Pictures/pienet-kuvat/" "**.mp4")
                  (filter #(= 0 (fs/size %)))
                  #_(filter #(fs/exists? (target-to-source-file-name (str %))))
                  (doall)))
  (count small)

  (target-to-fuzzy-source-path (str (first small))
                               #_"/Users/jukka/Pictures/pienet-kuvat/2018/2018-02-23/2018-02-23.18.48.16_4ca93a0f33a722da054a881c011e590d.MOV-small.mp4")
  "/Volumes/Backup_3_2/kuva-arkisto/2018/2018-02-23/2018-02-23.18.48.15_4ca93a0f33a722da054a881c011e590d.MOV"
  (->> small
       (filter #(fs/exists? (target-to-source-file-name (str %)))))

  (str (rand-nth small))
  (map str small)

  (doseq [target-path (map str small)]
    (prn target-path)
    (prn (str (target-to-fuzzy-source-path target-path)))

    (fs/delete-if-exists target-path)
    (resize-video (str (target-to-fuzzy-source-path target-path))
                  target-path)
    )

  (resize-file "/Volumes/Backup_3_1/kuva-arkisto/2003/2003-06-15/2003-06-15.12.43.36_e464a05f9104f56a8950ee124f3dc6aa.jpg" "/Users/jukka/Downloads/test.jpg")
  (resize "/Volumes/BACKUP1/kuva-arkisto/" "/Users/jukka/Pictures/minikuva-arkisto/")
  (resize "/Users/jukka/Downloads/uudet_kuvat/" "/Users/jukka/Downloads/arkisto_mini/")

  "/Volumes/Backup_3_2/kuva-arkisto/2018/2018-02-23/2018-02-23.18.47.40_5232fa4c6d8011e8d5a247ef74827e20.MOV: No such file or directory"
  "/Volumes/Backup_3_2/kuva-arkisto/2018/2018-02-23/2018-02-23.18.47.39_5232fa4c6d8011e8d5a247ef74827e20.MOV"
  )
