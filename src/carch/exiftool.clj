(ns carch.exiftool
  (:require [clojure.java.shell :as shell]
            [clojure.string :as string]
            [jsonista.core :as jsonista])
  (:use clojure.test))

(defn run-command [& args]
  (let [result (apply shell/sh args)]
    #_(println (:out result))

    (when (not= 0 (:exit result))
      (throw (Exception. (:err result))))

    (when (not= "" (:err result))
      (println "ERROR: " (:err result)))))

(defn parse-key-value [line]
  (let [matches (re-matches #"(.*): (.*)" (string/trim line))]
    [(nth matches 1)
     (nth matches 2)]))

(def exiftool-path "/Users/jukka/bin/Image-ExifTool-12.40/exiftool")

(deftest parse-key-value-test
  (is (= ["CreateDate" "2016:04:03 13:52:21"]
         (parse-key-value "CreateDate: 2016:04:03 13:52:21\n\n"))))

(defn- get-tag [file-name tag-name]
  (-> (with-out-str
        (run-command exiftool-path "-S" "-CreateDate" file-name))
      (parse-key-value)
      (second)))

(defn parse-date [string]
  (when string
       (let [[year month day hour minute second] (rest (re-matches #"(\d\d\d\d):(\d\d):(\d\d) (\d\d):(\d\d):(\d\d)"
                                                                   string))]
         {:year (Integer/parseInt year)
          :month (Integer/parseInt month)
          :day (Integer/parseInt day)
          :hour (Integer/parseInt hour)
          :minute (Integer/parseInt minute)
          :second (Integer/parseInt second)})))

(deftest parse-date-test
  (is (= {:year 2016,
          :month 4,
          :day 3,
          :hour 13,
          :minute 52,
          :second 21}
         (parse-date "2016:04:03 13:52:21"))))

(defn date-time-original [file-name]
  (-> (shell/sh exiftool-path "-json" "-EXIF:DateTimeOriginal" file-name)
      (:out)
      (jsonista/read-value)
      (first)
      (get "DateTimeOriginal")))

(defn media-create-date [file-name]
  (-> (shell/sh exiftool-path "-json" "-QuickTime:MediaCreateDate" file-name)
      (:out)
      (jsonista/read-value)
      (first)
      (get "MediaCreateDate")))

(defn subsecond-date-time-original [file-name]
  (-> (shell/sh exiftool-path "-json" "-Composite:SubSecDateTimeOriginal" #_"-EXIF:DateTimeOriginal" file-name)
      (:out)
      (jsonista/read-value)
      (first)
      (get "SubSecDateTimeOriginal" #_"DateTimeOriginal")))

(defn subsecond-date-time-orignal-to-map [exif-tool-date-time-original]
  (when exif-tool-date-time-original
    (let [[year month day hour minute second subsecond] (rest (re-matches #"(\d\d\d\d):(\d\d):(\d\d) (\d\d):(\d\d):(\d\d).(\d\d).*"
                                                                          exif-tool-date-time-original))]
      (when (and year month day hour minute second subsecond)
        {:year (Integer/parseInt year)
         :month (Integer/parseInt month)
         :day (Integer/parseInt day)
         :hour (Integer/parseInt hour)
         :minute (Integer/parseInt minute)
         :second (Integer/parseInt second)
         :subsecond (Integer/parseInt subsecond)}))))

(deftest test-exif-tool-date-time-orignal-to-map
  (is (= {:year 2021,
          :month 7,
          :day 20,
          :hour 17,
          :minute 35,
          :second 38,
          :subsecond 15}
         (subsecond-date-time-orignal-to-map "2021:07:20 17:35:38.15+03:00")))

  (is (= nil
         (subsecond-date-time-orignal-to-map nil))))

(defn get-date [file-name]
  (or (subsecond-date-time-orignal-to-map (subsecond-date-time-original file-name))
      (parse-date (date-time-original file-name))
      (parse-date (media-create-date file-name))))

(deftest test-get-date
  (is (= {:year 2020, :month 7, :day 5, :hour 14, :minute 53, :second 23}
         (get-date "dev-resources/Canon PowerShot SX260 HS.JPG")))

  (is (= {:year 2021, :month 7, :day 3, :hour 9, :minute 38, :second 42, :subsecond 83}
         (get-date "dev-resources/r6.cr3")))

  (is (= {:year 2021, :month 7, :day 20, :hour 19, :minute 9, :second 35, :subsecond 88}
         (get-date "dev-resources/iphone 12.heic")))

  (is (= {:year 2021, :month 7, :day 20, :hour 19, :minute 9, :second 35, :subsecond 88}
         (get-date "dev-resources/iphone 12.heic"))))


(comment
  (get-date "/Users/jukka/Pictures/uudet-kuvat/2021/2021-08-21/2021-08-21.09.46.13_6bb96a2482e3bfb1a859c34f43eb4f69.mp4")
  (subsecond-date-time-original #_"/Users/jukka/google-drive/src/carch/dev-resources/Canon PowerShot SX260 HS.JPG"
                                #_"/Users/jukka/google-drive/src/carch/dev-resources/r6.CR3"
                                "/Users/jukka/Pictures/uudet-kuvat/2021/2021-08-21/2021-08-21.09.46.13_6bb96a2482e3bfb1a859c34f43eb4f69.mp4")

  (media-create-date "/Users/jukka/Pictures/uudet-kuvat/2021/2021-08-21/2021-08-21.09.46.13_6bb96a2482e3bfb1a859c34f43eb4f69.mp4")
  (date-time-original "/Users/jukka/google-drive/src/carch/dev-resources/Canon PowerShot SX260 HS.JPG"
                      #_"/Users/jukka/google-drive/src/carch/dev-resources/r6.CR3")

  (get-tag "/Users/jukka/Downloads/IMG_0095.MOV" "CreateDate")
  (println (get-tag "/Users/jukka/Pictures/uudet_kuvat/100ANDRO/MOV_0006.mp4" "CreateDate"))
  (println (get-date "/Users/jukka/Pictures/uudet-kuvat/2021/2021-08-21/2021-08-21.09.46.13_6bb96a2482e3bfb1a859c34f43eb4f69.mp4"))

  (with-out-str
    (run-command "exiftool" "-S" "-CreateDate" "/Users/jukka/Pictures/uudet_kuvat/100ANDRO/MOV_0006.mp4"))


  (-> (shell/sh exiftool-path "-json" "/Users/jukka/google-drive/kadunvarsimainosten kuvat/IMG_3397.HEIC.jpg")
      (:out)
      (jsonista/read-value)
      ;; (first)
      ;; (get "SubSecDateTimeOriginal" #_"DateTimeOriginal")
      )

 
  )
