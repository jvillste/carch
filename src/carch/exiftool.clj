(ns carch.exiftool
  (:require
   [babashka.fs :as fs]
   [clojure.data :as data]
   [clojure.java.shell :as shell]
   [clojure.string :as string]
   [jsonista.core :as jsonista]
   [medley.core :as medley])
  (:import [java.time LocalDateTime
            ZonedDateTime
            ZoneId]
           [java.time.format DateTimeFormatter])
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

(def exiftool-path "exiftool")

(deftest parse-key-value-test
  (is (= ["CreateDate" "2016:04:03 13:52:21"]
         (parse-key-value "CreateDate: 2016:04:03 13:52:21\n\n"))))

(defn- get-tag [file-name tag-name]
  (-> (with-out-str
        (run-command exiftool-path "-S" "-CreateDate" file-name))
      (parse-key-value)
      (second)))

(defn parse-date [string]
  (try (when string
         (let [[year month day hour minute second] (rest (re-matches #"(\d\d\d\d):(\d\d):(\d\d) (\d\d):(\d\d):(\d\d)"
                                                                     string))]
           {:year (Integer/parseInt year)
            :month (Integer/parseInt month)
            :day (Integer/parseInt day)
            :hour (Integer/parseInt hour)
            :minute (Integer/parseInt minute)
            :second (Integer/parseInt second)}))
       (catch Exception _exception
         nil)))

(deftest parse-date-test
  (is (= {:year 2016,
          :month 4,
          :day 3,
          :hour 13,
          :minute 52,
          :second 21}
         (parse-date "2016:04:03 13:52:21"))))

(defn all-tags [file-name]
  (-> (shell/sh exiftool-path "-json" #_"-groupNames" file-name)
      (:out)
      ;;      (println)
      (jsonista/read-value)
      (first)
      ))

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

(defn parse-exif-timestamp [input]
  (let [helsinki-timestamp (.withZoneSameInstant (or (try
                                                       (ZonedDateTime/parse input
                                                                            (DateTimeFormatter/ofPattern "yyyy:MM:dd HH:mm:ssXXX"))
                                                       (catch Throwable _throwable
                                                         nil))

                                                     (.atZone (LocalDateTime/parse input
                                                                                   (DateTimeFormatter/ofPattern "yyyy:MM:dd HH:mm:ss"))
                                                              (ZoneId/of "UTC")))
                                                 (ZoneId/of "Europe/Helsinki"))]
    {:year (.getYear helsinki-timestamp)
     :month (.getMonthValue helsinki-timestamp)
     :day (.getDayOfMonth helsinki-timestamp)
     :hour (.getHour helsinki-timestamp)
     :minute (.getMinute helsinki-timestamp)
     :second (.getSecond helsinki-timestamp)}))

(defn get-date [file-name]
  (or (subsecond-date-time-orignal-to-map (subsecond-date-time-original file-name))
      (parse-exif-timestamp  (let [tags (all-tags file-name)]
                               (or (get tags "MediaCreateDate")
                                   (get tags "TrackCreateDate")
                                   (get tags "CreateDate")
                                   (get tags "DateTimeOriginal")
                                   (date-time-original file-name)
                                   (media-create-date file-name))))))

(deftest test-get-date

  (is (= {:year 2025, :month 9, :day 23, :hour 20, :minute 23, :second 59}
         (get-date "dev-resources/iphone 7 live photo movie.MOV")))

  (is (= {:year 2025, :month 9, :day 23, :hour 20, :minute 23, :second 58, :subsecond 89}
         (get-date "dev-resources/iphone 7 live photo.JPG")))

  (is (= {:year 2020, :month 7, :day 5, :hour 17, :minute 53, :second 23}
         (get-date "dev-resources/Canon PowerShot SX260 HS.JPG")))

  (is (= {:year 2021, :month 7, :day 3, :hour 9, :minute 38, :second 42, :subsecond 83}
         (get-date "dev-resources/r6.cr3")))

  (is (= {:year 2021, :month 7, :day 20, :hour 19, :minute 9, :second 35, :subsecond 88}
         (get-date "dev-resources/iphone 12.heic")))

  (is (= {:year 2021, :month 7, :day 20, :hour 19, :minute 9, :second 35, :subsecond 88}
         (get-date "dev-resources/iphone 12.heic"))))

(defn format-date-tag-string [{:keys [year month day hour minute second subsecond]}]
  (str year
       ":"
       (format "%02d" month)
       ":"
       (format "%02d" day)
       " "
       (format "%02d" hour)
       ":"
       (format "%02d" minute)
       ":"
       (format "%02d" second)
       (when subsecond
         (str "." (format "%02d" subsecond) "+03:00"))))

(deftest test-format-date-tag-string
  (is (= "2013:08:24 13:14:34"
         (format-date-tag-string {:year 2013, :month 8, :day 24, :hour 13, :minute 14, :second 34})))
  (is (= "2013:08:24 13:14:34.10+03:00"
         (format-date-tag-string {:year 2013, :month 8, :day 24, :hour 13, :minute 14, :second 34 :subsecond 10}))))

(defn write-date-time [file-name date-map]
  (doseq [tag ["MediaCreateDate"
               "MediaModifyDate"
               "TrackCreateDate"
               "CreateDate"
               "ModifyDate"
               "TrackModifyDate"]]
    (shell/sh exiftool-path "-overwrite_original" (str "-" tag "=" (format-date-tag-string (dissoc date-map
                                                                                                   :subsecond)))
              file-name)))

(defn copy-metadata [source-file target-file]
  (shell/sh exiftool-path "-overwrite_original" "-TagsFromFile" source-file "-all:all>all:all" target-file))

(comment
  (copy-metadata "/Users/jukka/Downloads/23.mp4"
                 "/Users/jukka/Downloads/2015-10-14.10.21.01_9c41bd689d9c3cdc6341e51aae64b900.mp4-small-target.mp4")

  (fs/copy "/Users/jukka/Downloads/2015-10-14.10.21.01_9c41bd689d9c3cdc6341e51aae64b900.mp4-small copy.mp4"
           "/Users/jukka/Downloads/2015-10-14.10.21.01_9c41bd689d9c3cdc6341e51aae64b900.mp4-small-target.mp4")

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

  (keys (->> (all-tags #_"/Volumes/LEENAN LEVY/kuvat/2013/2013-02-03/2013-02-03.11.54.45_ce2d8b287ffeafe51c99287a50ad1cb2.JPG"
                       "/Users/jukka/Downloads/23.mp4")
             (medley/filter-vals (fn [value]
                                   (and (some? value)
                                        (string? value)
                                        (parse-date value))))))

  (subsecond-date-time-original "/Users/jukka/Pictures/uudet-kuvat/2023/2023-12-08/2023-12-08.17.34.47.99_7f09ed666bbed156258247b113256889.CR3")

  (get-date "/Volumes/LEENAN LEVY/kuvat-arkisto-732/9. syyskuuta 1972/23.mp4")
  ;; => {:year 2015, :month 10, :day 14, :hour 10, :minute 21, :second 1}

  (all-tags "/Users/jukka/Downloads/23.mp4")
  (get-date "/Users/jukka/Downloads/2015-10-14.10.21.01_9c41bd689d9c3cdc6341e51aae64b900.mp4-small.mp4")

  (map parse-date (let [tags (all-tags "/Volumes/LEENAN LEVY/kuva-arkisto-4309/24. elokuuta 2013/40.MOV")]
                    [(get tags "MediaCreateDate")
                     (get tags "TrackCreateDate")
                     (get tags "CreateDate")
                     (get tags "DateTimeOriginal")]))
  (with-out-str
    (run-command "exiftool" "-S" "-CreateDate" "/Users/jukka/Pictures/uudet_kuvat/100ANDRO/MOV_0006.mp4"))


  (-> (shell/sh exiftool-path "-json" "/Users/jukka/google-drive/kadunvarsimainosten kuvat/IMG_3397.HEIC.jpg")
      (:out)
      (jsonista/read-value)
      ;; (first)
      ;; (get "SubSecDateTimeOriginal" #_"DateTimeOriginal")
      )

  (data/diff (all-tags "/Volumes/Backup_3_1/kuva-arkisto/2025/2025-09-29/2025-09-29.08.46.03.82_a526b45a5faecbd7007e016accb7c2b1.JPG")
             (all-tags "/Volumes/Backup_3_1/kuva-arkisto/2025/2025-09-29/2025-09-29.18.18.22.90_4cd8a234eb63e52a2c06d161383b1cf5.HEIC"))

  (all-tags "/Volumes/Backup_3_1/kuva-arkisto/2025/2025-09-29/2025-09-29.08.19.32.19_a4b95e426c90731d5f52cf0eb2895087.CR3")

  (= "iPhone 7 Plus"
     (get (all-tags "/Users/jukka/Pictures/pienet-kuvat/2025/2025-11-11/2025-11-11.14.31.09.22_89163cd9817a5d1ed8e5c646af5c1a2a.HEIC-small.jpg")
          "Model"))

  (all-tags "/Users/jukka/Pictures/pienet-kuvat/2025/2025-10-10/2025-10-10.13.24.57.87_3f7353232ae449007c924851afc7ea4b.CR3-small.jpg")
  (all-tags "/Users/jukka/Pictures/pienet-kuvat/2025/2025-11-07/2025-11-07.10.47.12_b483da9e60a831309e2319d2d4ecd317.MOV-small.mp4")
  )

(defn model [file-name]
  (get (all-tags file-name)
       "Model"))
