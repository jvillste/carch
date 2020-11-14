(ns carch.exiftool
  (:require [clojure.java.shell :as shell]
            [clojure.string :as string])
  (:use clojure.test))

(defn run-command [& args]
  (let [result (apply shell/sh args)]
    (println (:out result))

    (when (not= 0 (:exit result))
      (throw (Exception. (:err result))))

    (when (not= "" (:err result))
      (println "ERROR: " (:err result)))))

(defn parse-key-value [line]
  (let [matches (re-matches #"(.*): (.*)" (string/trim line))]
    [(nth matches 1)
     (nth matches 2)]))

(deftest parse-key-value-test
  (is (= ["CreateDate" "2016:04:03 13:52:21"]
         (parse-key-value "CreateDate: 2016:04:03 13:52:21\n\n"))))

(defn- get-tag [file-name tag-name]
  (-> (with-out-str
        (run-command "exiftool" "-S" "-CreateDate" file-name))
      (parse-key-value)
      (second)))

(defn parse-date [string]
  (let [[year month day hour minute second] (rest (re-matches #"(\d\d\d\d):(\d\d):(\d\d) (\d\d):(\d\d):(\d\d)"
                                                              string))]
    {:year (Integer/parseInt year)
     :month (Integer/parseInt month)
     :day (Integer/parseInt day)
     :hour (Integer/parseInt hour)
     :minute (Integer/parseInt minute)
     :second (Integer/parseInt second)}))

(deftest parse-date-test
  (is (= {:year 2016,
          :month 4,
          :day 3,
          :hour 13,
          :minute 52,
          :second 21}
         (parse-date "2016:04:03 13:52:21"))))

(defn get-date [file-name]
  (parse-date (get-tag file-name "CreateDate")))


(comment

  (get-tag "/Users/jukka/Downloads/IMG_0095.MOV" "CreateDate")
  (println (get-tag "/Users/jukka/Pictures/uudet_kuvat/100ANDRO/MOV_0006.mp4" "CreateDate"))
  (println (get-date "/Users/jukka/Downloads/temp/video/2016/2016-05-07/2016-04-03.13.52.21_7504ba4430f91de6b8f9450d44dfb32a.mp4"))

  (with-out-str
    (run-command "exiftool" "-S" "-CreateDate" "/Users/jukka/Pictures/uudet_kuvat/100ANDRO/MOV_0006.mp4")))
