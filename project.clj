(defproject com.nervestaple/lit-doc "0.1a"
  :description "A simple literate programming documentation generator"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [commons-logging/commons-logging "1.1.1"]
                 [hiccup "0.3.1"]
                 [org.clojars.mcav/pegdown "0.8.5.1"]]
  :dev-dependencies [[swank-clojure "1.2.0"]]
  :main com.nervestaple.lit-doc.main
  :resources-path "resources")
