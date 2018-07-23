(defproject org.clojars.mihaelkonjevic/content-store "0.0.3"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.339"]
                 [cljsjs/moment "2.22.2-0"]
                 [keechma/forms "0.1.3"]
                 [com.stuartsierra/dependency "0.2.0"]]

  :min-lein-version "2.5.3"

  :source-paths ["src/clj" "src/cljs"]

  :plugins [[lein-cljsbuild "1.1.4"]]

  :clean-targets ^{:protect false} ["resources/public/js"
                                    "target"
                                    "test/js"]

  :figwheel {}

  :profiles
  {:dev
   {:dependencies []

    :plugins      [[lein-figwheel "0.5.15"]
                   [lein-doo "0.1.8"]]}}

  :cljsbuild
  {:builds
   [{:id           "test"
     :source-paths ["src/cljs" "test/cljs"]
     :compiler     {:output-to     "resources/public/js/test.js"
                    :output-dir    "resources/public/js/test"
                    :main          content-store.runner
                    :optimizations :none}}]})
