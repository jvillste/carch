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

(defn add-quotes [value]
  (str "\"" value "\""))

(defn resize-file [source-path target-path & [{:keys [maximum-size format-options format] :or {maximum-size 2000 format-options "50" format "jpeg"}}]]
  #_(println "sips" "-s" "format" (add-quotes format) "-s" "formatOptions" (add-quotes format-options) "-Z" (str maximum-size) (add-quotes source-path) "--out" (add-quotes target-path))

  (let [dimensions (image-dimensions source-path)
        result (shell/sh "magick" source-path "-resize" (str maximum-size "x" maximum-size "\\>") "-quality" format-options target-path)
        #_(if (> maximum-size (apply max (vals dimensions))) ;; -Z scales up images even if it should not
            (shell/sh "sips" "-s" "format" format "-s" "formatOptions" format-options source-path "--out" target-path)
            (shell/sh "sips" "-s" "format" format "-s" "formatOptions" format-options "-Z" (str maximum-size) source-path "--out" target-path))
        #_(shell/sh "/Users/jukka/bin/imagemagick/bin/convert" "-quality" "50" "-resize" "2000x2000" source-path target-path)]
    (when (not (= 0 (:exit result)))
      (println "Error when resizing:" (:err result))
      (throw (ex-info "Error when resizing" result)))
    result))

(defn resize-photo-for-blue-ray [source-path target-path]
  (resize-file source-path target-path
               {:maximum-size 500
                :format-options "40"}))

(comment
  (let [parameters {:source
                    {:name "lumo" :file-name "2025-03-27.07.20.37.07_9729de83c702e742de215d6a1632c2e2.JPG-small.jpg"}
                    #_{:name "puut" :file-name "2026-02-05.15.29.20.03_792fa81e1dfa8d0b666d7ad74f7c1e4b.CR3-small.jpg"}
                    :maximum-size 500
                    :format-options "100"
                    :format #_"heic" #_"jp2" "jpeg"}]
    (resize-file (str "/Users/jukka/Downloads/source-video/" (-> parameters :source :file-name))
                 (str "/Users/jukka/Downloads/target-video/"
                      (string/join "-" [(-> parameters :source :name)
                                        (:maximum-size parameters)
                                        (:format-options parameters)
                                        (:format parameters)])
                      ".jpg")
                 parameters))
  ) ;; TODO: remove me

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

(defn resize-video [source-path target-path & [{:keys [crf codec preset video-filter audio-bitrate]
                                                :or {crf "28"
                                                     codec "libx265"
                                                     preset "slow"
                                                     video-filter "scale='min(800,iw)':-2"
                                                     audio-bitrate "128k"}}]]
  ;; (println "ffmpeg -y " (string/join " "
  ;;                                    (for [[label value] (partition 2 ["-i" source-path
  ;;                                                                      "-c:v" codec
  ;;                                                                      "-crf" crf
  ;;                                                                      "-preset" preset
  ;;                                                                      "-c:a" "aac"
  ;;                                                                      "-b:a" audio-bitrate
  ;;                                                                      "-tag:v" "hvc1"
  ;;                                                                      "-vf" video-filter])]
  ;;                                      (str label " \"" value "\"")))
  ;;          (str "\"" target-path "\""))

  (let [result (jx-java-shell/timeout-sh (* 20 (size-in-megabytes source-path))
                                         "ffmpeg"
                                         "-y"
                                         "-i" source-path
                                         "-c:v" codec
                                         "-crf" crf
                                         "-preset" preset
                                         "-c:a" "aac"
                                         "-b:a" audio-bitrate
                                         "-tag:v" "hvc1"
                                         "-vf" video-filter
                                         target-path)]
    (when (not (= 0 (:exit result)))
      (println "Error when resizing:" (:err result))
      (throw (ex-info "Error when resizing" result)))
    result))

(defn resize-video-for-blue-ray [source-path target-path]
  (resize-video source-path target-path
                {:crf "35"
                 :codec "libx265" #_"libaom-av1"
                 :preset #_"ultrafast", #_"superfast", #_"veryfast", #_"faster", #_"fast", "medium", #_"slow", #_"slower", #_"veryslow"
                 :resolution "580"
                 :audio-bitrate "20k" #_"50k" #_"98k" #_"128k"}))

(comment

  ;; vaaka synttärit /Users/jukka/Pictures/pienet-kuvat/2025/2025-11-21/2025-11-21.18.25.28_cab7cae90ad792e5e50ff8062081fb7c.MOV-small.mp4
  ;; vaaka pojat /Users/jukka/Pictures/pienet-kuvat/2017/2017-12-23/2017-12-23.07.19.04_b7f3097e857a7100a203a393ac0f4da4.MOV-small.mp4
  ;; pysty siru /Users/jukka/Pictures/pienet-kuvat/2023/2023-03-20/2023-03-20.18.56.26_2e85bfc6eb66430efdd5f7ede9c15e3f.MOV-small.mp4
  (let [parameters {:source
                    {:name "vaaka-pojat-pieni" :file-name "2017-12-23.07.19.04_b7f3097e857a7100a203a393ac0f4da4.MOV-small.mp4"}
                    #_{:name "vaaka-synttärit-pieni" :file-name "2025-11-21.18.25.28_cab7cae90ad792e5e50ff8062081fb7c.MOV-small.mp4"}
                    #_{:name "pysty-siru-pieni" :file-name "2023-03-20.18.56.26_2e85bfc6eb66430efdd5f7ede9c15e3f.MOV-small.mp4"}

                    #_{:name "vaaka-pojat" :file-name "2017-12-23.07.19.04_b7f3097e857a7100a203a393ac0f4da4.MOV"}
                    #_{:name "pysty-siru" :file-name "2023-03-20.18.56.26_2e85bfc6eb66430efdd5f7ede9c15e3f.MOV.mp4"}
                    #_{:name "vaaka-synttärit" :file-name "2025-11-21.18.25.28_cab7cae90ad792e5e50ff8062081fb7c.MOV"}


                    :crf "35"
                    :codec "libx265" #_"libaom-av1"
                    :preset #_"ultrafast", #_"superfast", #_"veryfast", #_"faster", #_"fast", "medium", #_"slow", #_"slower", #_"veryslow"
                    :resolution "580"
                    :audio-bitrate #_"20k" "30k" #_"50k" #_"98k" #_"128k"}]
    (resize-video (str "/Users/jukka/Downloads/source-video/" (-> parameters :source :file-name))
                  (str "/Users/jukka/Downloads/target-video/"
                       (string/join "-" [(-> parameters :source :name)
                                         (:codec parameters)
                                         (:preset parameters)
                                         (:crf parameters)
                                         (:resolution parameters)
                                         (:audio-bitrate parameters)])
                       ".mp4")
                  (assoc parameters
                         :video-filter (str "scale='min(" (:resolution parameters) ",iw)':'min(" (:resolution parameters)  ",ih)':force_original_aspect_ratio=decrease"))))

  ;; 2xBD-R 0.29411766
  {:crf "35"
   :codec "libx265" #_"libaom-av1"
   :preset #_"ultrafast", #_"superfast", #_"veryfast", #_"faster", #_"fast", "medium", #_"slow", #_"slower", #_"veryslow"
   :resolution "580"
   :audio-bitrate "20k" #_"50k" #_"98k" #_"128k"}

  (float (/ 204 25))

  (float (/ 1.2 (/ 204 25)))
  ;; => 0.14705883

  (float (* 2 (/ 1.2 (/ 204 25))))
  ;; => 0.29411766

  ) ;; TODO: remove me


;; "/Users/jukka/Pictures/pienet-kuvat/2025/2025-12-24/2025-12-24.19.34.44.14_27ba525e890dd1d2c60649f786b11e1b.MP4-small.mp4"
;; "ffmpeg -i input.mp4 -vf "scale=-2:1080" -c:v libx265 -preset slow -crf 22 -c:a copy output.mp4"

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

  (resize-video "/Users/jukka/Pictures/pienet-kuvat/2025/2025-12-24/2025-12-24.19.34.44.14_27ba525e890dd1d2c60649f786b11e1b.MP4-small.mp4"
                "/Users/jukka/Downloads/video.mp4")

  (float (/ (fs/size "/Users/jukka/Pictures/uudet-lajittelemattomat-kuvat/DCIM/ZoomerangVideos/zoomerang_1674239084640.mp4")
            1000000))

  (do
    (fs/delete "/Users/jukka/Downloads/small.mp4")
    (resize-video "/Users/jukka/Pictures/uudet-lajittelemattomat-kuvat/DCIM/ZoomerangVideos/zoomerang_1674239084640.mp4"
                  "/Users/jukka/Downloads/small.mp4"))

  (time (do (fs/delete "/Users/jukka/Downloads/small.mp4")
            (resize-video "/Users/jukka/Pictures/uudet-lajittelemattomat-kuvat/DCIM/ZoomerangVideos/zoomerang_1678008665573.mp4"
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
