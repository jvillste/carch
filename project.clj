(defproject carch "1.0.0-SNAPSHOT"
  :description "A media archiver"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [com.drewnoakes/metadata-extractor "2.16.0"]
                 [metosin/jsonista "0.3.3"]
                 [clj-time "0.15.2"]
                 [babashka/fs "0.4.19"]
                 [timeout-shell "1.0.0"]
                 [dev.weavejester/medley "1.7.0"]]
  :javac-options ["-cp" "local-jars/metadata-extractor-2.3.1.jar"]
  :profiles {:uberjar {:aot :all}}
  :main carch.main)
