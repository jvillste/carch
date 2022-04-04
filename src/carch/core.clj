(ns carch.core
  (:require [carch.common :as common]
            [carch.exiftool :as exiftool]
            [clojure.java.shell :as shell]
            [clojure.java.io :as io]
            [carch.resize :as resize]
            [clj-time.core :as clj-time]
            [clj-time.coerce :as coerce]
            [jsonista.core :as jsonista]
            [clojure.string :as string])
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
  (thread-count [archiver])
  (archiver-name [archiver])
  (accept-source-file [archiver file])
  (target-file-name [archiver md5 temp-file])
  (target-path [archiver temp-file-name])
  (copy-file [archiver source-file-name target-file-names])
  (compare-file-sizes? [archiver]))


(defn- optimal-thread-count []
  (+ (.. Runtime getRuntime availableProcessors)
     2))

(comment
  6 "Elapsed time: 53828.515847 msecs"
  4 "Elapsed time: 60741.127826 msecs"
  8 "Elapsed time: 50822.612694 msecs"
  10 "Elapsed time: 29816.971341 msecs"
  ) ;; TODO: remove-me

(def executor (memoize (fn [thread-count] (java.util.concurrent.Executors/newFixedThreadPool thread-count))))

(defn- map-concurrently [thread-count function vals]
  (let [executor (executor thread-count)]
    (->> vals
         (mapv #(fn [] (function %)))
         (.invokeAll executor)
         (mapv deref))))

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
          (sort-by #(.getAbsolutePath %)
                   (common/files-in-directory directory-path))))

(defn extension [file-name]
  (.substring file-name (+ 1 (.lastIndexOf file-name "."))))

(deftest test-extension
  (is (= "bar"
         (extension "foo.bar"))))


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

(defn file-name-date-string [{:keys [year month day hour minute second subsecond]}]
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
       (format "%02d" second)
       (when subsecond
         (str "." (format "%02d" subsecond)))))

(deftest test-file-name-date-string
  (is (= "2021-07-20.19.09.01.01"
         (file-name-date-string {:year 2021, :month 7, :day 20, :hour 19, :minute 9, :second 1, :subsecond 1})))

  (is (= "2021-07-20.19.09.35"
         (file-name-date-string {:year 2021, :month 7, :day 20, :hour 19, :minute 9, :second 35}))))


(defn bytes-to-hex-string [bytes]
  (apply str (map #(format "%02x" %)
                  bytes)))

(def log-lock (Object.))

(defn write-log [& values]
  (locking log-lock
    (apply println values))

  #_(swap! log (fn [old-log] (str old-log "\n" (apply str message)))))


(defn process-file [source-file-name processor]
  (try (let [buffer (byte-array (* 1024 1024))]
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

    (write-log "copying to" (append-paths (target-path archiver source-file-name)
                                          (target-file-name archiver md5 source-file-name)))
    (doseq [target-file-name (filter target-exists
                                     target-file-names)]
      (write-log source-file-name "already exists in" target-file-name))

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

(deftest test-photo-exif-date
  (is (= #inst "2019-10-14T09:00:48.000-00:00"
         (photo-exif-date "dev-resources/image.jpg"))))

(defn photo-exif-datemap [photo-file-name]
  (let [date (try (or (exiftool/get-date photo-file-name)
                      (date-to-map (photo-exif-date photo-file-name)))
                  (catch Exception e
                    (println "WARNING: could not get exif date from " photo-file-name)
                    nil))]
    (when (nil? date)
      (println "WARNING: could not get exif date from " photo-file-name))

    date))

(defn photo-date [file-name]
  (or (photo-exif-datemap file-name)
      (file-creation-date file-name)))

(def photo-extension-set #{"dng" "png" "cr2" "cr3" "nef" "jpg" "tif" "heic"})

(deftype PhotoArchiver []
  Archiver

  (thread-count [archiver] 1)

  (archiver-name [archiver] "photos")

  (accept-source-file [archiver file]
    (contains? photo-extension-set
               (.toLowerCase (extension (.getName file)))))

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

(defn photo-file-name-for-xmp-file-name [xmp-file-name]
  (string/replace xmp-file-name
                  "xmp"
                  (->> photo-extension-set
                       (map (fn [photo-extension]
                              (string/replace xmp-file-name
                                              ".xmp"
                                              (str "." photo-extension))))
                       (filter (fn [file-name]
                                 (.exists (File. file-name))))
                       (first)
                       (extension))))

(comment
  (photo-file-name-for-xmp-file-name "/Users/jukka/Downloads/test-photos/2021-10-14.08.27.56.32_39ce985ea7e6848015fab2efa8509e13.xmp")
  ) ;; TODO: remove-me


(deftype XMPArchiver []
  Archiver

  (thread-count [archiver] 1)

  (archiver-name [archiver] "xmp files")

  (accept-source-file [archiver file]
    (= "xmp" (.toLowerCase (extension (.getName file)))))

  (target-file-name [archiver xmp-md5 source-file-name]
    (let [photo-file-name (photo-file-name-for-xmp-file-name source-file-name)]
      (file-name (photo-date photo-file-name)
                 (md5 photo-file-name)
                 "xmp")))

  (target-path [archiver source-file-name]
    (-> source-file-name
        photo-file-name-for-xmp-file-name
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

  (thread-count [archiver] 1)

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


(deftype ResizingPhotoArchiver []
  Archiver

  (thread-count [archiver] (optimal-thread-count))

  (archiver-name [archiver] "resized photos")

  (accept-source-file [archiver file]
    (#{"dng" "png" "cr2" "cr3" "nef" "jpg" "tif" "heic"} (.toLowerCase (extension (.getName file)))))

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
  (or (exiftool/get-date file-name)
      (file-creation-date file-name)))

(comment
  (exiftool/get-date #_"/Users/jukka/Downloads/IMG_0095.MOV"
                     #_"/Users/jukka/Pictures/uudet-kuvat/r6 4/7Y7A2551.MP4"
                     "/Users/jukka/Pictures/uudet-kuvat/jarmon pokkari/DCIM/822_0507/MVI_3691.MOV")

  (get-video-date #_"/Users/jukka/Downloads/IMG_0095.MOV"
                  #_"/Users/jukka/Pictures/uudet-kuvat/r6 4/7Y7A2551.MP4"
                  "/Users/jukka/Pictures/uudet-kuvat/jarmon pokkari/DCIM/822_0507/MVI_3691.MOV"))

(deftype VideoArchiver []
  Archiver

  (thread-count [archiver] 1)

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

(deftype ResizingVideoArchiver []
  Archiver

  (thread-count [archiver] 1)

  (archiver-name [archiver] "resized videos")

  (accept-source-file [archiver file]
    (#{"mov" "mp4" "avi" "wmv" "mpg"} (.toLowerCase (extension (.getName file)))))

  (target-file-name [archiver md5 source-file-name]
    (file-name (get-video-date source-file-name)
               md5
               (str (extension source-file-name) "-small.mp4")))

  (target-path [archiver source-file-name]
    (-> source-file-name
        get-video-date
        target-path-by-date))

  (copy-file [archiver source-file-name target-file-names]
    (doseq [target-file-name target-file-names]
      (resize/resize-video source-file-name target-file-name)))

  (compare-file-sizes? [archiver] false))

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


(comment
  (compare "a" "a")
  ) ;; TODO: remove-me

(defn start [{:keys [source-paths archive-paths last-processed-file-path]} archivers]

  (let [copy-lock (Object.)
        last-successfully-processed-file-path-atom (atom nil)]
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
                                 (println "shutting down")
                                 (when @running
                                   (println "stopping copying")
                                   (reset! running false)
                                   (locking copy-lock
                                     (Thread/sleep 1000) ;; wait for error report printout in the copying thread
                                     (println "exiting"))))))

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
             (let [batch-size (thread-count archiver)]
               (write-log "********* Archiving " (count (files-for-archiver archiver source-path)) " " (archiver-name archiver) " from " source-path " *********")
               (let [files (files-for-archiver archiver source-path)
                     files (if last-processed-file-path
                             (do (write-log "skipping to " last-processed-file-path)
                                 (drop-while (fn [file]
                                               (< 0
                                                  (compare last-processed-file-path
                                                           (.getPath file))))
                                             files))
                             files)
                     file-count (count files)]
                 (loop [files files
                        index 1]
                   (when (seq files)
                     (locking copy-lock
                       (write-log (str "Processing files " index "-" (min file-count
                                                                          (dec (+ index batch-size))) "/" file-count ":\n"
                                       (string/join "\n"
                                                    (map #(.getPath %)
                                                         (take batch-size files)))))
                       (try
                         (map-concurrently batch-size
                                           (fn [file]
                                             (archive archiver
                                                      (.getPath file)
                                                      archive-paths)
                                             (reset! last-successfully-processed-file-path-atom (.getPath (first files))))
                                           (take batch-size files))
                         (catch Exception exception
                           (swap! files-with-errors conj (.getPath (first files)))
                           (write-log "ERROR in archiving file " (.getPath (first files)) " : " (.getMessage exception))
                           (write-log (stack-trace exception)))))

                     (when @running
                       (recur (drop batch-size files) (+ index batch-size))))))))

           (if @running
             (do (write-log "Archiving ready.")
                 (reset! running false))
             (write-log "Archiving stopped by the user."))
           (write-log (count @files-with-errors) "files had errors:")
           (doseq [file-name @files-with-errors]
             (write-log file-name))
           (write-log "last successfully processed file: " @last-successfully-processed-file-path-atom))
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
                                                                      (dissoc (photo-date (.getPath file))
                                                                              :subsecond))))))
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

  (photo-exif-datemap #_"/Users/jukka/Pictures/uudet-kuvat/r6 5/7Y7A2767.CR3"
                      #_"/Users/jukka/Pictures/uudet-kuvat/550d/DCIM/100CANON/_MG_7891.CR2"
                      #_"/Users/jukka/Pictures/uudet-kuvat/jarmon pokkari/DCIM/822_0507/IMG_3667.JPG"
                      "/Users/jukka/Downloads/IMG_3042.HEIC")

  (stop)

  (println (source-directories))

  (start {:source-paths ["/Volumes/Backup_3_1/kuva-arkisto/2020"]

          :archive-paths ["/Users/jukka/Pictures/pienet-kuvat"]}
         [(->ResizingPhotoArchiver)])

  (start {:source-paths [#_"/Volumes/Backup_3_1/kuva-arkisto/2021/2021-12-25"
                         "/Volumes/Backup_3_1/kuva-arkisto/2006"
                         #_"/Users/jukka/Pictures/uudet-kuvat/2022/2022-03-29"]
          :archive-paths ["/Users/jukka/Downloads/small-videos"]}
         [(->ResizingVideoArchiver)])

  (start {:source-paths ["/Volumes/Backup_3_1/kuva-arkisto/2018"]
          :archive-paths ["/Users/jukka/Pictures/pienet-kuvat"]
          :last-processed-file-path "/Volumes/Backup_3_1/kuva-arkisto/2018/2018-01-14/2018-01-14.14.41.50_6195e9e9e7f5f5076bb089b6d7f0dbb8.mp4"}
         [(->ResizingVideoArchiver)])


  (time (start {:source-paths ["/Users/jukka/Downloads/test-photos"]
                :archive-paths ["/Users/jukka/Downloads/target"]}
               [#_(->ResizingPhotoArchiver) (->PhotoArchiver) #_(->VideoArchiver) (->XMPArchiver)
                ]))

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

  (.getImageMeta (ExifTool.)
                 (File. "/Users/jukka/Pictures/uudet_kuvat/100ANDRO/DSC_0244.JPG" #_"/Users/jukka/Pictures/uudet_kuvat/100ANDRO/MOV_0006.mp4")
                 (into-array [com.thebuzzmedia.exiftool.ExifTool$Tag/DATE_TIME_ORIGINAL]))

  (command-line-ui)
  (stop)

  (println "{:source-paths [\"/Volumes/Backup_3_1/kuva-arkisto/2018\"] :archive-paths [\"/Users/jukka/Pictures/pienet-kuvat\"] :last-processed-file-path \"/Volumes/Backup_3_1/kuva-arkisto/2018/2018-01-14/2018-01-14.14.41.50_6195e9e9e7f5f5076bb089b6d7f0dbb8.mp4\"}")
  )
