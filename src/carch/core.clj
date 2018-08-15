(ns carch.core
  (:require [carch.common :as common]
            [carch.exiftool :as exiftool]
            [clojure.java.shell :as shell]
            [clojure.java.io :as io])
  (:import [java.io File]
           [java.util Calendar Date]
           [com.drew.imaging.jpeg JpegMetadataReader]
           [com.drew.metadata.exif ExifSubIFDDirectory]
           [java.nio.file.attribute BasicFileAttributes]
           [java.nio.file Files Path LinkOption])
  
  (:use clojure.test))


(def file-progress (atom 0))
(def running (atom true))
(def log (atom ""))
(def files-with-errors (atom []))

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
  (swap! log (fn [old-log] (str old-log "\n" (apply str message)))))

(def buffer (byte-array (* 1024 1024 20)))

(defn process-file [source-file-name processor]
  (let [file-size (.length (File. source-file-name))]
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

(defn archive [archiver source-file-name archive-paths]
  (let [message-digest (java.security.MessageDigest/getInstance "MD5")
        temp-file-names (map #(append-paths % (str "archiver.temp." (extension source-file-name)))
                             archive-paths)]

    (dorun (map delete-if-exists temp-file-names))

    (let [output-streams (map clojure.java.io/output-stream temp-file-names)]
      (try
        (process-file source-file-name
                      (fn [buffer bytes-read]
                        (.update message-digest buffer 0 bytes-read)
                        (dorun (pmap #(.write % buffer 0 bytes-read)
                                     output-streams))))
        (finally
          (dorun (map #(.close %) output-streams)))))

    (let [md5 (bytes-to-hex-string (.digest message-digest))]
      (doseq [archive-path archive-paths]
        (let [temp-file-name (append-paths archive-path (str "archiver.temp." (extension source-file-name)))]
          (when @running
            (do (.setLastModified (File. temp-file-name)
                                  (.lastModified (File. source-file-name)))
                (let [target-path (append-paths archive-path
                                                (target-path archiver temp-file-name))
                      target-file-name (append-paths target-path
                                                     (target-file-name archiver md5 temp-file-name))
                      aae-source-file-name (change-file-extension source-file-name "AAE")]

                  (when (.exists (File. aae-source-file-name))
                    (io/copy (io/file aae-source-file-name)
                             (io/file (change-file-extension target-file-name "AAE"))))

                  (if (.exists (File. target-file-name))
                    (write-log "allready exists " target-file-name)
                    (do (.mkdirs (File. target-path))
                        (move-file temp-file-name
                                   target-file-name))))))
          (delete-if-exists temp-file-name))))))

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
  (let [exif-directory (-> (File. photo-file-name)
                           (JpegMetadataReader/readMetadata)
                           (.getDirectory ExifSubIFDDirectory))]
    (if (.containsTag exif-directory ExifSubIFDDirectory/TAG_DATETIME_DIGITIZED)
      (date-to-map (.getDate exif-directory  ExifSubIFDDirectory/TAG_DATETIME_DIGITIZED))
      nil)))

(defn photo-date [file-name]
  (try (photo-exif-date file-name)
       (catch Exception e
         (file-creation-date file-name))))


(deftype PhotoArchiver []
  Archiver

  (archiver-name [archiver] "photos")

  (accept-source-file [archiver file]
    (#{"cr2" "nef" "jpg" "tif"} (.toLowerCase (extension (.getName file)))))

  (target-file-name [archiver md5 temp-file-name]
    (file-name (photo-date temp-file-name)
               md5
               (extension temp-file-name)))

  (target-path [archiver temp-file-name]
    (-> temp-file-name
        photo-date
        target-path-by-date)))


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

  (target-file-name [archiver md5 temp-file-name]
    (file-name (get-video-date temp-file-name) md5 (extension temp-file-name)))

  (target-path [archiver temp-file-name]
    (-> temp-file-name
        get-video-date
        target-path-by-date)))


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

(defn start [{:keys [source-paths archive-paths]}]
  (println "starting")
  (doseq  [archive-path archive-paths]
    (when (not (.exists (File. archive-path)))
      (throw (Exception. (str "The archive path " archive-path " does not exist.")))))

  (reset! running true)
  (try (let [archivers [(->PhotoArchiver) (->VideoArchiver)]
             source-paths (or source-paths
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
                 (write-log "Archiving file " index "/" file-count " " (.getPath (first files)))
                 (try (archive archiver (.getPath (first files)) archive-paths)
                      (catch Exception exception
                        (swap! files-with-errors conj (.getPath (first files)))
                        (write-log "ERROR in archiving file " (.getPath (first files)) " : " (.getMessage exception))
                        (write-log (stack-trace exception))))

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
         (write-log (stack-trace exception)))))

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
             (do (print value "%  \r")
                 (flush)))
           -1))
  (Thread/sleep 1000)
  (if @running
    (recur)
    (do (println @log)
        (println "stopped"))))


(comment
  (stop)
  
  (println (source-directories))

  (.run (Thread. (fn []
                   (start {:source-paths ["/Users/jukka/Google Drive/gfx/juw/kuvia2" "/Users/jukka/Google Drive/gfx/juw/kuvia"]

                           :archive-paths ["/Volumes/Backup 2 1/kuva-arkisto" "/Volumes/Backup 2 2/kuva-arkisto"]}))))


  (start {:source-paths ["/Users/jukka/Downloads/source"]
          :archive-paths ["/Users/jukka/Downloads/target"]})



  (photo-date "/Users/jukka/Pictures/DSC_0002.JPG")
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
