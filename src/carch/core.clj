(ns carch.core
  (:require [carch.common :as common]
            [carch.exiftool :as exiftool]
            [clojure.java.shell :as shell]
            [clojure.java.io :as io]
            [carch.resize :as resize]
            [clj-time.core :as clj-time]
            [clj-time.coerce :as coerce])
  (:import [java.io File]
           [java.util Calendar Date]
           [com.drew.imaging ImageMetadataReader]
           [com.drew.imaging.jpeg JpegMetadataReader]
           [com.drew.metadata.exif ExifSubIFDDirectory]
           [java.nio.file.attribute BasicFileAttributes]
           [java.nio.file Files Path Paths LinkOption]
           sun.misc.Signal
           sun.misc.SignalHandler)
  (:use clojure.test))


(def running (atom true))
(def log (atom ""))
(def files-with-errors (atom []))

(defprotocol Archiver
  (archiver-name [archiver])
  (accept-source-file [archiver file])
  (target-file-name [archiver md5 temp-file])
  (target-path [archiver temp-file-name])
  (copy-file [archiver source-file-name target-file-names])
  (compare-file-sizes? [archiver]))

(defn date-to-map [date]
  (let [date (coerce/from-date date)]
    {:year (clj-time/year date)
     :month (clj-time/month date)
     :day (clj-time/day date) #_ (.get calendar Calendar/DAY_OF_MONTH)
     :hour (clj-time/hour date)
     :minute (clj-time/minute date)
     :second (clj-time/second date)}))

(deftest test-date-to-map
  (is (= {:year 2020, :month 7, :day 8, :hour 21, :minute 5, :second 24}
         (date-to-map #inst "2020-07-08T21:05:24.000-00:00"))))

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

(defn source-directories []
  (map #(.getPath %)
       (filter #(-> (File. (str (.getPath %) "/DCIM"))
                    (.exists))
               (roots))))

(defn files-for-archiver [archiver directory-path]
  (filter #(accept-source-file archiver %)
          (common/files-in-directory directory-path)))

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
  (apply println message)
  #_(swap! log (fn [old-log] (str old-log "\n" (apply str message)))))

(def buffer (byte-array (* 1024 1024 20)))

(defn process-file [source-file-name processor]
  (try (let [file-size (.length (File. source-file-name))]
         (with-open [input-stream (clojure.java.io/input-stream source-file-name)]
           (try
             (loop [bytes-read (.read input-stream buffer)
                    total-bytes-read bytes-read]
               (when (> bytes-read 0)
                 (processor buffer bytes-read)
                 (when @running
                   (recur (.read input-stream buffer)
                          (+ total-bytes-read bytes-read)))))
             (catch Throwable e
               (println "exception during file processing " e)
               (throw e)))))))

(defn move-file [source-file-name target-file-name]
  (.renameTo (File. source-file-name)
             (File. target-file-name)))

(defn stack-trace [exception]
  (let [string-writer (java.io.StringWriter.)]
    (.printStackTrace exception (java.io.PrintWriter. string-writer))
    (.toString string-writer)))

(defn delete-if-exists [file-name]
  (when (.exists (File. file-name))
    (.delete (File. file-name))))

(defn remove-file-extension
  "Remove a file extension from a string."
  [filename]
  (.substring filename 0 (.lastIndexOf filename ".")))

(defn change-file-extension [source-file-name new-extension]
  (str (remove-file-extension source-file-name)
       "."
       new-extension))

(defn md5 [file-name]
  (let [message-digest (java.security.MessageDigest/getInstance "MD5")]
    (process-file file-name
                  (fn [buffer bytes-read]
                    (.update message-digest buffer 0 bytes-read)))
    (bytes-to-hex-string (.digest message-digest))))

(defn file-length [file-name]
  (.length (File. file-name)))

(defn- copy-file-with-streams [source-file-name target-file-names]
  (let [output-streams (map clojure.java.io/output-stream target-file-names)]
    (try
      (process-file source-file-name
                    (fn [buffer bytes-read]
                      (dorun (pmap #(.write % buffer 0 bytes-read)
                                   output-streams))))
      (finally
        (dorun (map #(.close %) output-streams))))))

(defn- resize-file [source-file-name target-file-names]
  (doseq [target-file-name target-file-names]
    (resize/resize-file source-file-name target-file-name)))

(defn archive [archiver source-file-name archive-paths]
  (let [md5 (md5 source-file-name)
        source-length (file-length source-file-name)
        target-file-names (for [archive-path archive-paths]
                            (append-paths archive-path
                                          (target-path archiver source-file-name)
                                          (target-file-name archiver md5 source-file-name)))
        target-exists #(and (.exists (File. %))
                            (or (not (compare-file-sizes? archiver))
                                (= (file-length %)
                                   source-length)))]
    (println "copying to" (append-paths (target-path archiver source-file-name)
                                        (target-file-name archiver md5 source-file-name)))
    (doseq [target-file-name (filter target-exists
                                     target-file-names)]
      (println source-file-name "already exists in" target-file-name))

    (let [target-file-names (remove target-exists
                                    target-file-names)]
      (run! #(.mkdirs (File. (.getParent (File. %))))
            target-file-names)
      (copy-file archiver source-file-name target-file-names))))

(defn file-name [date md5 extension]
  (str (if date
         (str (file-name-date-string date) "_")
         "")
       md5 "." extension))

(defn file-last-modified-date [file-name]
  (date-to-map (Date. (.lastModified (File. file-name)))))

(defn file-creation-date [file-name]
  (date-to-map (Date/from (.toInstant (get (Files/readAttributes (.toPath (io/file file-name))
                                                                 "*"
                                                                 (into-array LinkOption [])
                                                                 #_(.class BasicFileAttributes))
                                           "creationTime")))))

#_(file-creation-date "/Users/jukka/Dropbox/bin/kuvat_kortilta_macissa copy.clj")


;; PHOTOS

(defn photo-exif-date [photo-file-name]
  (some (fn [[directory tag]]
          (when (= "Date/Time Original"
                   (.getTagName tag))
            (.getDate directory (.getTagType tag))))
        (for [directory (-> (File. photo-file-name)
                            (ImageMetadataReader/readMetadata)
                            (.getDirectories))
              tag (.getTags directory)]
          [directory tag])))
(comment
  (photo-exif-date "/Volumes/Backup_3_1/kuva-arkisto/2020/2020-07-30/2020-07-30.16.57.04_32e0ff3afef390fc74668abde371f3d5.HEIC")
  ) ;; TODO: remove-me

(deftest test-photo-exif-date
  (is (= #inst "2019-10-14T09:00:48.000-00:00"
         (photo-exif-date "dev-resources/image.jpg"))))

(defn photo-exif-datemap [photo-file-name]
  (try (date-to-map (photo-exif-date photo-file-name))
       (catch Exception e
         (println "WARNING: could not get exif date from " photo-file-name)
         nil)))

(defn photo-date [file-name]
  (or (photo-exif-datemap file-name)
      (file-creation-date file-name)))

(deftype PhotoArchiver []
  Archiver

  (archiver-name [archiver] "photos")

  (accept-source-file [archiver file]
    (#{"dng" "png" "cr2" "nef" "jpg" "tif" "heic"} (.toLowerCase (extension (.getName file)))))

  (target-file-name [archiver md5 source-file-name]
    (file-name (photo-date source-file-name)
               md5
               (extension source-file-name)))

  (target-path [archiver source-file-name]
    (-> source-file-name
        photo-date
        target-path-by-date))

  (copy-file [archiver source-file-name target-file-names]
    (copy-file-with-streams source-file-name target-file-names))

  (compare-file-sizes? [archiver] true))

(defn date-from-file-name [file-name]
  (let [[year month day hour minute second] (rest (re-matches #"(\d\d\d\d)-(\d\d)-(\d\d)\.(\d\d)\.(\d\d)\.(\d\d).*"
                                                              file-name))]
    (when (and year month day hour minute second)
     {:year (Integer/parseInt year)
      :month (Integer/parseInt month)
      :day (Integer/parseInt day)
      :hour (Integer/parseInt hour)
      :minute (Integer/parseInt minute)
      :second (Integer/parseInt second)})))

(deftest test-date-from-file-name
  (is (= {:year 2020, :month 8, :day 25, :hour 14, :minute 51, :second 7}
         (date-from-file-name "2020-08-25.14.51.07_d504c307052c6edff89294be1c4140b3.HEIC")))

  (is (= nil
         (date-from-file-name "/foo/2020-08-25.14.51.07_d504c307052c6edff89294be1c4140b3.HEIC"))))

(defn date-is-wrong? [file]
  (and (= "heic"
          (.toLowerCase (extension (.getName file))))
       (not (= (date-from-file-name (.getName file))
               (photo-date (.getPath file))))))

(deftype WrongDatePhotoArchiver []
  Archiver

  (archiver-name [archiver] "photos")

  (accept-source-file [archiver file]
    (date-is-wrong? file))

  (target-file-name [archiver md5 source-file-name]
    (file-name (date-from-file-name (.getName (File. source-file-name)))
               md5
               (extension source-file-name)))

  (target-path [archiver source-file-name]
    (-> source-file-name
        photo-date
        target-path-by-date))

  (copy-file [archiver source-file-name target-file-names]
    (copy-file-with-streams source-file-name target-file-names))

  (compare-file-sizes? [archiver] true))

(comment
  "/Users/jukka/Documents/wrong-date-photos"

  ) ;; TODO: remove-me

(defn resized-file-date [source-file-path]
  (or (date-from-file-name (.getName (File. source-file-path)))
      (photo-date source-file-path)))

(deftype ResizingPhotoArchiver []
  Archiver

  (archiver-name [archiver] "resized photos")

  (accept-source-file [archiver file]
    (#{;;"dng" "png" "cr2" "nef" "jpg" "tif"
       "heic"} (.toLowerCase (extension (.getName file)))))

  (target-file-name [archiver md5 source-file-name]
    (file-name (photo-date source-file-name)
               md5
               (str (extension source-file-name) "-small.jpg")))

  (target-path [archiver source-file-name]
    (-> source-file-name
        photo-date
        target-path-by-date))

  (copy-file [archiver source-file-name target-file-names]
    (resize-file source-file-name target-file-names))

  (compare-file-sizes? [archiver] false))

;; VIDEOS

(defn get-video-date [file-name]
  (try (exiftool/get-date file-name)
       (catch Exception e
         (file-creation-date file-name))))

(comment
  (exiftool/get-date "/Users/jukka/Downloads/IMG_0095.MOV")
  (get-video-date "/Users/jukka/Downloads/IMG_0095.MOV"))

(deftype VideoArchiver []
  Archiver

  (archiver-name [archiver] "videos")

  (accept-source-file [archiver file]
    (#{"avi" "mov" "mp4" "mpg" "wmv"} (.toLowerCase (extension (.getName file)))))

  (target-file-name [archiver md5 source-file-name]
    (file-name (get-video-date source-file-name) md5 (extension source-file-name)))

  (target-path [archiver source-file-name]
    (-> source-file-name
        get-video-date
        target-path-by-date))

  (copy-file [archiver source-file-name target-file-names]
    (copy-file-with-streams source-file-name target-file-names))

  (compare-file-sizes? [archiver] true))


;; UI

(defn counts [archivers source-paths]
  (apply str
         (flatten (for [source-path source-paths]
                    [source-path " :"
                     (for [archiver archivers]
                       [" "
                        (count (files-for-archiver archiver source-path))
                        " "
                        (archiver-name archiver)])
                     "\n"]))))

(deftest counts-test
  (is (= (counts [(->PhotoArchiver) (->VideoArchiver)]
                 ["/foo"
                  "foo2"])
         "/foo : 0 photos 0 videos\nfoo2 : 0 photos 0 videos\n")))

(comment
  (let [lock-object (Object.)]
    (.start (Thread. (locking lock-object
                       (Thread/sleep 1000))))
    (locking lock-object)
    :ready)
  ) ;; TODO: remove-me


(defn start [{:keys [source-paths archive-paths]} archivers]

  (let [copy-lock (Object.)]
    #_(Signal/handle (Signal. "HUP")
                     (proxy [SignalHandler] []
                       (handle [sig]
                         (println "stopping copying")
                         (reset! running false)
                         (locking copy-lock)
                         (println "exiting")
                         (System/exit 0))))

    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. (fn []
                                 (println "stopping copying")
                                 (reset! running false)
                                 (locking copy-lock
                                   (println "exiting")))))

    (doseq  [archive-path archive-paths]
      (when (not (.exists (File. archive-path)))
        (throw (Exception. (str "The archive path " archive-path " does not exist.")))))

    (reset! running true)
    (try (let [source-paths (or source-paths
                                (source-directories))]
           (write-log "Archiving from " (vec source-paths) " to " (vec archive-paths))

           (write-log (counts archivers source-paths))

           (doseq [archiver archivers
                   source-path source-paths]
             (write-log "********* Archiving " (count (files-for-archiver archiver source-path)) " "(archiver-name archiver) " from " source-path " *********")
             (let [files (files-for-archiver archiver source-path)
                   file-count (count files)]
               (loop [files files
                      index 1]
                 (when (seq files)
                   (locking copy-lock
                     (write-log "Archiving file " index "/" file-count " " (.getPath (first files)))
                     (try (archive archiver (.getPath (first files)) archive-paths)
                          (catch Exception exception
                            (swap! files-with-errors conj (.getPath (first files)))
                            (write-log "ERROR in archiving file " (.getPath (first files)) " : " (.getMessage exception))
                            (write-log (stack-trace exception)))))

                   (when @running
                     (recur (rest files) (inc index)))))))

           (if @running
             (write-log "Archiving ready.")
             (write-log "Archiving stopped by the user."))
           (write-log (count @files-with-errors) "files had errors:")
           (doseq [file-name @files-with-errors]
             (write-log file-name)))
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
  (Thread/sleep 1000)
  (if @running
    (recur)
    (do (println @log)
        (println "stopped"))))

(defn process! [reducible & transducers]
  (transduce (apply comp transducers)
             (constantly nil)
             reducible))

(deftest test-process
  (let [result-atom (atom [])]
    (process! (range 10)
              (take 6)
              (filter even?)
              (map #(swap! result-atom conj %)))
    (is (= [0 2 4] @result-atom))))

(defonce count-atom (atom 0))

(comment

  (do (println "starting")
      (reset! count-atom 0)
      (let [archive-root "/Users/jukka/Pictures/pienet-kuvat/"]
        (process! (common/file-reducible (str archive-root "/2020"))
                  (filter (fn [file]
                            (and (.endsWith (.toLowerCase (.getName file))
                                            "heic-small.jpg"))))
                  #_(take 10)
                  (map (fn [file]
                         (swap! count-atom inc)
                         (println (.getPath file))
                         (.delete (File. (.getPath file)))))))
      (println "done"))



  (def deletion-future (future (do (println "starting")
                                   (reset! count-atom 0)
                                   (let [wrong-date-photo-archiver (->WrongDatePhotoArchiver)
                                         photo-archiver (->PhotoArchiver)
                                         archive-root "/Volumes/backup2/kuva-arkisto/"]
                                     (process! (common/file-reducible (str archive-root "/2020"))
                                               (filter (fn [file]
                                                         (and (= "heic"
                                                                 (.toLowerCase (extension (.getName file))))
                                                              (not (= (date-from-file-name (.getName file))
                                                                      (photo-date (.getPath file)))))))
                                               #_(take 10)
                                               (map (fn [file]
                                                      (swap! count-atom inc)
                                                      (let [md5-hash (md5 (.getPath file))
                                                            path (.getPath file)
                                                            right-date-archive-file-name (str archive-root
                                                                                              (target-path photo-archiver
                                                                                                           (.getPath file))
                                                                                              "/"
                                                                                              (target-file-name photo-archiver
                                                                                                                md5-hash
                                                                                                                (.getPath file)))
                                                            wrong-date-archive-file-name (str "/Users/jukka/Documents/wrong-date-photos/"
                                                                                              (target-path wrong-date-photo-archiver
                                                                                                           (.getPath file))
                                                                                              "/"
                                                                                              (target-file-name wrong-date-photo-archiver
                                                                                                                md5-hash
                                                                                                                (.getPath file)))]
                                                        (println "has wrong date:")
                                                        (println path)
                                                        (println right-date-archive-file-name)
                                                        (println wrong-date-archive-file-name)
                                                        (println (.exists (File. wrong-date-archive-file-name))
                                                                 (.exists (File. right-date-archive-file-name)))
                                                        (when (and (.exists (File. wrong-date-archive-file-name))
                                                                   (.exists (File. right-date-archive-file-name)))
                                                          (println "deleting")
                                                          (.delete (File. path))))))))
                                   (println "done"))

                               ))

  (count (common/files-in-directory #_"/Users/jukka/Pictures/pienet-kuvat/2020/2020-10-22"
                                    #_"/Volumes/Backup_3_1/kuva-arkisto/2020/2020-04-27"
                                    #_"/Volumes/Backup_3_1/kuva-arkisto/2020"
                                    "/Volumes/backup/kuva-arkisto/2020"
                                    #_"/Users/jukka/Pictures/pienet-kuvat/2020/2020-10-22"
                                    #_"/Volumes/Backup_3_1/kuva-arkisto/2020/2020-10-21"))
  @count-atom
  (realized? deletion-future)
  (future-cancelled? deletion-future)
  (future-cancel deletion-future)

  (.delete (File. "/Users/jukka/Downloads/image001.png"))
  (->> (common/files-in-directory "/Volumes/Backup_3_1/kuva-arkisto/2020")
       (filter (fn [file]
                 (.contains (.toLowerCase (.getName file))
                            "051ad1c176a378dcf618232c97844764")))
       (map (fn [file]
              (.getPath file)))
       (first))


  (photo-exif-datemap "/Users/jukka/Pictures/pienet-kuvat/2020/2020-10-22/2020-10-22.00.30.51_051ad1c176a378dcf618232c97844764.JPG-small.jpg")
  (md5 "/Users/jukka/Pictures/uudet_kuvat/sirun iphone/IMG_8240.MOV")
  (archive2 (->PhotoArchiver)
            "/Users/jukka/Pictures/uudet_kuvat/sirun iphone/IMG_0105.HEIC"
            ["/Users/jukka/Downloads/archive1" "/Users/jukka/Downloads/archive2"])

  (stop)

  (println (source-directories))

  (start {:source-paths ["/Volumes/Backup_3_1/kuva-arkisto/2020"]

          :archive-paths ["/Users/jukka/Pictures/pienet-kuvat"]}
         [(->ResizingPhotoArchiver)])


  (start {:source-paths ["/Users/jukka/Downloads/source"]
          :archive-paths ["/Users/jukka/Downloads/target"]}
         [(->PhotoArchiver) (->VideoArchiver)])

  (start {:source-paths [#_"/Volumes/Backup_3_1/kuva-arkisto/2020/2020-04-27"
                         "/Volumes/Backup_3_1/kuva-arkisto/2020"]
          #_["/Volumes/Backup_3_1/kuva-arkisto/2020/2020-10-21" "/Volumes/Backup_3_1/kuva-arkisto/2020"]
          :archive-paths ["/Users/jukka/Documents/wrong-date-photos"]}
         [(->WrongDatePhotoArchiver)])

  (pr-str {:source-paths ["/Users/jukka/Downloads/heicit"]
           #_["/Volumes/Backup_3_1/kuva-arkisto/2020/2020-10-21" "/Volumes/Backup_3_1/kuva-arkisto/2020"]
           :archive-paths ["/Volumes/backup/kuva-arkisto"
                           "/Volumes/backup2/kuva-arkisto"
                           "/Volumes/Backup_3_1/kuva-arkisto"]})

  ;;  java -jar ~/bin/carch-1.0.0-SNAPSHOT-standalone.jar copy "{:source-paths [\"/Users/jukka/Downloads/heicit\"], :archive-paths [\"/Volumes/backup/kuva-arkisto\" \"/Volumes/backup2/kuva-arkisto\" \"/Volumes/Backup_3_1/kuva-arkisto\"]}"

  (file-last-modified-date "/Volumes/NEW VOLUME/DCIM/789CANON/IMG_8953.cr2")
  (file-last-modified-date "/Volumes/NEW VOLUME/DCIM/789CANON/IMG_8953.jpg")

  (with-out-str
    (run-command "exiftool" "-CreateDate" "/Users/jukka/Pictures/uudet_kuvat/100ANDRO/MOV_0006.mp4"))

  (.getImageMeta (ExifTool.)
                 (File. "/Users/jukka/Pictures/uudet_kuvat/100ANDRO/DSC_0244.JPG" #_"/Users/jukka/Pictures/uudet_kuvat/100ANDRO/MOV_0006.mp4")
                 (into-array [com.thebuzzmedia.exiftool.ExifTool$Tag/DATE_TIME_ORIGINAL]))

  (command-line-ui)
  (stop)

  )
