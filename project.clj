(defproject tg-rss-bot "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "The MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/data.json "0.2.6"]
                 [clj-http "2.0.0"]
                 [clj-yaml "0.4.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/core.match "0.3.0-alpha4"]]
  :main ^:skip-aot tg-rss-bot.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
