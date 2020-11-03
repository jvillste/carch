(defproject carch "1.0.0-SNAPSHOT"
  :description "A media archiver"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.drewnoakes/metadata-extractor "2.12.0"]]
  :javac-options     ["-cp" "local-jars/metadata-extractor-2.3.1.jar"]
  :profiles {:uberjar {:aot :all}}
  :main carch.main)
