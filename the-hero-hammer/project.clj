(defproject the-hero-hammer "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [hiccup "1.0.5"]
                 [com.taoensso/nippy "2.10.0"]
                 [compojure "1.1.8"]
                 [http-kit "2.1.18"]
                 [debugger "0.1.7"]
                 [org.clojure/clojurescript "1.7.228"]
                 [co.paralleluniverse/pulsar "0.7.4"]
                 [clj-time "0.11.0"]
                 [org.clojure/data.json "0.2.6"]
                 [cljs-ajax "0.5.3"]]
  :java-agents [[co.paralleluniverse/quasar-core "0.7.4"]]
  :plugins [[lein-cljsbuild "1.1.2"]]
  :main the-hero-hammer.core
  :aot [the-hero-hammer.core]
  :uberjar {:aot :all}
  :cljsbuild {
    :builds [{
      :source-paths ["src-cljs"]
      :compiler {
         :output-to "resources/public/js/main.js"
         :optimizations :whitespace
         :pretty-print true
         }
      }]
    }
  )
