(defproject snake "0.1.0-SNAPSHOT"
  :description "An implementation of snake in ClojureScript and HTML5 Canvas"

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2138"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]]

  :plugins [[lein-cljsbuild "1.0.2"]]

  :source-paths ["src"]

  :cljsbuild { 
    :builds [{:id "snake"
              :source-paths ["src"]
              :compiler {
                :output-to "snake.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})
