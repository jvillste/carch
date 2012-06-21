(ns carch.core
  (:import [java.io File]
           [java.util Calendar Date]
           [com.drew.imaging.jpeg JpegMetadataReader]
           [com.drew.metadata.exif ExifDirectory]))

(def file-progress (atom 0))
(def running (atom true))
(def log (atom ""))

(defprotocol Archiver
  (archiver-name [archiver])
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
    "Windows XP" :windows
    (throw (Exception. (str "Unknown operating system: " (System/getProperty "os.name")) ))))

(defn roots []
  (case (operating-system)
    :linux (-> (File. "/media")
               (.listFiles))
    :windows (File/listRoots)))

(defn source-directory []
  (.getPath (first (filter #(-> (File. (str (.getPath %) "/DCIM"))
                                (.exists))
                           (roots)))))

(defn files-in-directory [directory-path]
  (reduce (fn [files file] (if (.isDirectory file)
                             (concat files (files-in-directory (.getPath file)))
                             (conj files file)))
          []
          (.listFiles (File. directory-path))))

(defn files-for-archiver [archiver directory-path]
  (filter #(accept-source-file archiver %)
          (files-in-directory directory-path)))

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

(defn file-name-date-string [{:keys [year month day hour minute second]}]
  (str year
       "-"
       (format "%02d" month)
       "-"
       (format "%02d" day)
       "."
       (format "%02d" hour)
       "."
       (format "%02d" minute)
       "."
       (format "%02d" second)))


(defn bytes-to-hex-string [bytes]
  (apply str (map #(format "%02x" %)
                  bytes)))

(defn write-log [& message]
  (swap! log (fn [old-log] (str old-log "\n" (apply str message)))))

(defn process-file [source-file-name processor]
  (let [buffer (byte-array 1024)
        file-size (.length (File. source-file-name))]
    (with-open [input-stream (clojure.java.io/input-stream source-file-name)]
      (loop [bytes-read (.read input-stream buffer)
             total-bytes-read bytes-read]
        (when (> bytes-read 0)
          (reset! file-progress (int (* 100
                                        (/ total-bytes-read
                                           file-size))))
          (processor buffer bytes-read)
          (when @running
            (recur (.read input-stream buffer)
                   (+ total-bytes-read bytes-read))))))))

(defn move-file [source-file-name target-file-name]
  (.renameTo (File. source-file-name)
             (File. target-file-name)))

(defn stack-trace [exception]
  (let [string-writer (java.io.StringWriter.)]
    (.printStackTrace exception (java.io.PrintWriter. string-writer))
    (.toString string-writer)))

(defn archive [archiver source-file-name archive-path]
  (let [message-digest (java.security.MessageDigest/getInstance "MD5")
        temp-file-name (append-paths archive-path (str "archiver.temp." (extension source-file-name)))]
    (when (.exists (File. temp-file-name))
      (.delete (File. temp-file-name)))
    (with-open [output-stream (clojure.java.io/output-stream temp-file-name)]
      (process-file source-file-name
                    (fn [buffer bytes-read]
                      (.update message-digest buffer 0 bytes-read)
                      (.write output-stream buffer 0 bytes-read)))
      (when @running
        (do (.setLastModified (File. temp-file-name)
                              (.lastModified (File. source-file-name)))
            (let [md5 (bytes-to-hex-string (.digest message-digest))
                  target-path (append-paths archive-path
                                            (target-path archiver temp-file-name))
                  target-file-name (append-paths target-path
                                                 (target-file-name archiver md5 temp-file-name))]
              (if (.exists (File. target-file-name))
                (do (write-log "allready exists " target-file-name)
                    (.delete (File. temp-file-name)))

                (do (.mkdirs (File. target-path))
                    (move-file temp-file-name
                               target-file-name))))))
      (when (.exists (File. temp-file-name))
        (.delete (File. temp-file-name))))))

(defn file-name [date md5 extension]
  (str (if date
         (str (file-name-date-string date) "_")
         "")
       md5 "." extension))

;; PHOTOS

(defn photo-date [photo-file-name]
  (let [exif-directory (-> (File. photo-file-name)
                           (JpegMetadataReader/readMetadata)
                           (.getDirectory ExifDirectory))]
    (if (.containsTag exif-directory ExifDirectory/TAG_DATETIME_DIGITIZED)
      (date-to-map (.getDate exif-directory  ExifDirectory/TAG_DATETIME_DIGITIZED))
      nil)))


(deftype JPGArchiver []
  Archiver

  (archiver-name [archiver] "kuvaa")

  (accept-source-file [archiver file]
    (= (.toLowerCase (extension (.getName file)))
       "jpg"))

  (target-file-name [archiver md5 temp-file-name]
    (file-name (photo-date temp-file-name) md5 "jpg"))

  (target-path [archiver temp-file-name]
    (-> temp-file-name
        photo-date
        target-path-by-date)))

;; VIDEOS

(defn video-date [video-file-name]
  (date-to-map (Date. (.lastModified (File. video-file-name)))))


(deftype VideoArchiver []
  Archiver

  (archiver-name [archiver] "videota")

  (accept-source-file [archiver file]
    (#{"avi" "mov"} (.toLowerCase (extension (.getName file)))))

  (target-file-name [archiver md5 temp-file-name]
    (file-name (video-date temp-file-name) md5 (extension temp-file-name)))

  (target-path [archiver temp-file-name]
    (append-paths "video"
                  (-> temp-file-name
                      video-date
                      target-path-by-date))))


;; UI

(defn start
  ([archive-path]
     (try (start (source-directory) archive-path)
          (catch Exception exception
            (write-log "ERROR in archiving: " (.getMessage exception))
            (write-log (stack-trace exception)))))

  ([source-path archive-path]
     (reset! running true)
     (try (let [archivers [(JPGArchiver.) (VideoArchiver.)]]
            (write-log "Archiving from " source-path " to " archive-path)
            (doseq [archiver archivers]
              (write-log (str (count (files-for-archiver archiver source-path))
                              " "
                              (archiver-name archiver))))
            (doseq [archiver archivers]
              (write-log "********* " (archiver-name archiver) " *********")
              (let [files (files-for-archiver archiver source-path)
                    file-count (count files)]
                (loop [files files
                       index 1]
                  (when (seq files)
                    (write-log "Archiving file " index "/" file-count " " (.getPath (first files)))
                    (archive archiver (.getPath (first files)) archive-path)
                    (when @running
                      (recur (rest files) (inc index)))))))
            (if @running
              (write-log "Archiving ready.")
              (write-log "Archiving stopped by the user.")))
          (catch Exception exception
            (write-log "ERROR in archiving: " (.getMessage exception))
            (write-log (stack-trace exception))))))

(defn stop []
  (reset! running false))

(defn command-line-ui []
  (swap! log
         (fn [log-contents]
           (when (> (count log-contents)
                    0)
             (println log-contents))
           ""))
  (swap! file-progress
         (fn [value]
           (when (> value 0)
             (println value "%"))
           -1))
  (Thread/sleep 1000)
  (if @running
    (recur)
    (do (println @log)
        (println "stopped"))))

(comment
  (doseq [archiver [(JPGArchiver.) (VideoArchiver.)]
          file (files-for-archiver archiver (File. "/home/jukka/Downloads/kuvakoe"))]
    (archive archiver (.getPath file) "/home/jukka/Downloads/kuva-arkisto"))

  (archive (VideoArchiver.) "/media/Kingston/DCIM/115___06/MVI_4599.MOV" "/media/LaCie/kuva-arkisto")

  (println (source-directory))

  (stop)
  (start "/media/Kingston" "/media/LaCie/kuva-arkisto")
(start "/media/LaCie/kuva-arkisto")
(command-line-ui)


  (println (target-path (JPGArchiver.) "/media/Kingston/DCIM/115___06/IMG_4566.JPG"))

  (println (photo-date  "/media/Kingston/DCIM/115___06/IMG_4566.JPG"))

  (println (files-for-archiver (JPGArchiver.) (File. "/home/jukka/Downloads")))
  )