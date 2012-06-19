(ns carch.core
  (:import [java.io File]
           [java.util Calendar Date]
           [com.drew.imaging.jpeg JpegMetadataReader]
           [com.drew.metadata.exif ExifDirectory]))


(defprotocol Archiver
  (accept-source-file [archiver file])
  (target-file-name [archiver md5 temp-file])
  (target-path [archiver temp-file-name]))

(defn date-to-map [date]
  (let [calendar (Calendar/getInstance)]
    (.setTime calendar date)
    {:year (.get calendar Calendar/YEAR)
     :month (+ 1 (.get calendar Calendar/MONTH))
     :day (.get calendar Calendar/DAY_OF_MONTH)
     :hour (.get calendar Calendar/HOUR_OF_DAY)
     :minute (.get calendar Calendar/MINUTE)
     :second (.get calendar Calendar/SECOND)}))

(defn operating-system []
  (case (System/getProperty "os.name")
    "Linux" :linux
    "Windows" :windows
    :unknown))

(defn roots []
  (case (operating-system)
    :linux (-> (File. "/media")
               (.listFiles))
    :windows (File/listRoots)))

(defn source-directory []
  (first (filter #(-> (File. (str (.getPath %) "/DCIM"))
                      (.exists))
                 (roots))))

(defn files-in-directory [directory]
  (reduce (fn [files file] (if (.isDirectory file)
                             (concat files (files-in-directory file))
                             (conj files file)))
          []
          (.listFiles directory)))

(defn files-for-archiver [archiver directory]
  (filter #(accept-source-file archiver %)
          (files-in-directory directory)))

(defn extension [file-name]
  (.substring file-name (+ 1 (.lastIndexOf file-name "."))))


(defn append-paths [& paths]
  (apply str (interpose File/separator
                        paths)))

(defn target-path-by-date [date]
  (if date
    (let [{:keys [year month day]} date]
      (str year
           File/separator
           year
           "-"
           (format "%02d" month)
           "-"
           (format "%02d" day)))
    "dateless"))

(defn file-name-date-string [{:keys [year month day hour minute]}]
  (str year
       "-"
       (format "%02d" month)
       "-"
       (format "%02d" day)
       "-"
       (format "%02d" hour)
       "-"
       (format "%02d" minute)))

(defn bytes-to-hex-string [bytes]
  (apply str (map #(format "%02x" %)
                  bytes)))

(defn process-file [source-file-name processor]
  (let [buffer (byte-array 1024)]
    (with-open [input-stream (clojure.java.io/input-stream source-file-name)]
      (loop [bytes-read (.read input-stream buffer)]
        (when (> bytes-read 0)
          (processor buffer bytes-read)
          (recur (.read input-stream buffer)))))))

(defn move-file [source-file-name target-file-name]
  (.renameTo (File. source-file-name)
             (File. target-file-name)))

(defn archive [archiver source-file-name archive-path]
  (let [message-digest (java.security.MessageDigest/getInstance "MD5")
        temp-file-name (append-paths archive-path (str "archiver.temp." (extension source-file-name)))]
    (with-open [output-stream (clojure.java.io/output-stream temp-file-name)]
      (process-file source-file-name
                    (fn [buffer bytes-read]
                      (.update message-digest buffer 0 bytes-read)
                      (.write output-stream buffer 0 bytes-read)))
      (.setLastModified (File. temp-file-name)
                        (.lastModified (File. source-file-name)))
      (let [md5 (bytes-to-hex-string (.digest message-digest))
            target-path (append-paths archive-path
                                      (target-path archiver temp-file-name))
            target-file-name (append-paths target-path
                                           (target-file-name archiver md5 temp-file-name))]
        (if (.exists (File. target-file-name))
          (do (println "allready exists " target-file-name)
              (.delete (File. temp-file-name)))

          (do (.mkdirs (File. target-path))
              (move-file temp-file-name
                         target-file-name)))))))


(defn file-name [date md5 extension]
  (if date
    (str (file-name-date-string date) "-" md5 "." extension)
    (str md5 "." extension)))

;; PHOTOS

(defn photo-date [photo-file-name]
  (let [directory (-> (File. photo-file-name)
                      (JpegMetadataReader/readMetadata)
                      (.getDirectory ExifDirectory)
                      )]
    (if (.containsTag directory ExifDirectory/TAG_DATETIME_DIGITIZED)
      (date-to-map (.getDate ExifDirectory/TAG_DATETIME_DIGITIZED))
      nil)))


(deftype JPGArchiver []
  Archiver

  (accept-source-file [archiver file]
    (= (.toLowerCase (extension (.getName file)))
       "jpg"))

  (target-file-name [archiver md5 temp-file-name]
    (file-name (photo-date temp-file-name) md5 "jpg"))

  (target-path [archiver temp-file-name]
    (append-paths "photos"
                  (-> temp-file-name
                      photo-date
                      target-path-by-date))))

;; VIDEOS

(defn video-date [video-file-name]
  (date-to-map (Date. (.lastModified (File. video-file-name)))))


(deftype VideoArchiver []
  Archiver

  (accept-source-file [archiver file]
    (#{"avi" "mov"} (.toLowerCase (extension (.getName file)))))

  (target-file-name [archiver md5 temp-file-name]
    (file-name (video-date temp-file-name) md5 (extension temp-file-name)))

  (target-path [archiver temp-file-name]
    (append-paths "videos"
                  (-> temp-file-name
                      video-date
                      target-path-by-date))))


(comment
(doseq [archiver [(JPGArchiver.) (VideoArchiver.)]
          file (files-for-archiver archiver (File. "/home/jukka/Downloads/kuvakoe"))]
    (archive archiver (.getPath file) "/home/jukka/Downloads/kuva-arkisto"))

  (archive (VideoArchiver.) "/media/Kingston/DCIM/115___06/MVI_4599.MOV" "/home/jukka/Downloads/kuva-arkisto")



  (target-path (JPGArchiver.) "/home/jukka/Downloads/kuvakoe/conspare-side2.jpg")

(println (photo-date "/home/jukka/Downloads/kuvakoe/conspare-side2.jpg"))

  (println (files-for-archiver (JPGArchiver.) (File. "/home/jukka/Downloads")))
  )