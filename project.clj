(defproject quil-cljs-fig "0.1.0-SNAPSHOT"
  :description "Clojurescript implementation of a popular puzzle board game"
  :url "https://github.com/ivansalazar/qwircl"
  :license {:name "GNU General Public License v3.0"
            :url "https://github.com/ivansalazar/qwircl/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [quil "2.7.1"]
                 [org.clojure/clojurescript "1.10.238"]]

  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-figwheel "0.5.15"]]
  :hooks [leiningen.cljsbuild]

  :cljsbuild
  {:builds [; development build with figwheel hot swap
            {:id "development"
             :source-paths ["src"]
             :figwheel true
             :compiler
             {:main "qwircl.core"
              :output-to "resources/public/js/main.js"
              :output-dir "resources/public/js/development"
              :asset-path "js/development"}}
            ; minified and bundled build for deployment
            {:id "optimized"
             :source-paths ["src"]
             :compiler
             {:main "qwircl.core"
              :output-to "resources/public/js/main.js"
              :output-dir "resources/public/js/optimized"
              :asset-path "js/optimized"
              :optimizations :advanced}}]})
