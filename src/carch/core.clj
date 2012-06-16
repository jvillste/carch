(ns carch.core
  (:import [java.io File]
           [java.util Calendar]
           [com.drew.imaging.jpeg JpegMetadataReader]
           [com.drew.metadata.exif ExifDirectory]))

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

(defn files-by-extension [directory extension]
  (filter #(.endsWith (.toLowerCase (.getName %)) (.toLowerCase extension))
          (files-in-directory directory)))

(defn photo-date [photo-file]
  (-> photo-file
      (JpegMetadataReader/readMetadata)
      (.getDirectory ExifDirectory)
      (.getDate ExifDirectory/TAG_DATETIME_DIGITIZED)))

(defn extension [file-name]
  (.substring file-name (+ 1 (.lastIndexOf file-name "."))))

(defn file-type [file]
  (case (.toLowerCase (extension (.getName file)))
    "jpg" :jpg
    "mov" :mov
    "avi" :avi))

(defn append-paths [& paths]
  (apply str (interpose File/separator
                        paths)))

(defn date-to-map [date]
  (let [calendar (Calendar/getInstance)]
    (.setTime calendar date)
    {:year (.get calendar Calendar/YEAR)
     :month (+ 1 (.get calendar Calendar/MONTH))
     :day (.get calendar Calendar/DAY_OF_MONTH)
     :hour (.get calendar Calendar/HOUR_OF_DAY)
     :minute (.get calendar Calendar/MINUTE)
     :second (.get calendar Calendar/SECOND)}))

(defn target-path-by-date [date]
  (let [{:keys [year month day]} (date-to-map date)]
    (str year
         File/separator
         year
         "-"
         (format "%02d" month)
         "-"
         (format "%02d" day))))

(defn file-name-date-string [date]
  (let [{:keys [year month day hour minute]} (date-to-map date)]
    (str year
         "-"
         (format "%02d" month)
         "-"
         (format "%02d" day)
         "-"
         (format "%02d" hour)
         "-"
         (format "%02d" minute))))

(defn target-path [file]
  (case (file-type file)
    :jpg (let [date (photo-date file)]
           (if date
             (target-path-by-date date)
             "dateless"))
    :mov "videos"
    :avi  "videos"))

(defn target-file-name [file md5]
  (case (file-type file)
    :jpg (let [date (photo-date file)]
           (if date
             (str (file-name-date-string date) "-" md5 ".jpg")
             (str md5 ".jpg")))
    :mov "videos"
    :avi "videos"))


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

(defn process [source-file-name archive-path]
  (let [message-digest (java.security.MessageDigest/getInstance "MD5")
        temp-file-name (append-paths archive-path (str "archiver.temp." (extension source-file-name)))]
    (with-open [output-stream (clojure.java.io/output-stream temp-file-name)]
      (process-file source-file-name
                    (fn [buffer bytes-read]
                      (.update message-digest buffer 0 bytes-read)
                      (.write output-stream buffer 0 bytes-read)))
      (let [md5 (bytes-to-hex-string (.digest message-digest))
            target-file-name (append-paths archive-path
                                           (target-path (File. temp-file-name))
                                           (target-file-name (File. temp-file-name)))]
        (.mkdirs (File. (target-path (File. temp-file-name))))
        (move-file temp-file-name
                   target-file-name)))))