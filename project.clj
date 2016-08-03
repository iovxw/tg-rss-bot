(defproject tg-rss-bot "0.3.2"
  :description "Yet another RSS bot for Telegram"
  :url "https://github.com/iovxw/tg-rss-bot"
  :license {:name "The MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.json "0.2.6"]
                 [clj-http "3.1.0"]
                 [org.clojure/java.jdbc "0.3.5"]
                 [org.xerial/sqlite-jdbc "3.7.2"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojars.dive-networks/feedparser-clj "0.5.3"]]
  :main ^:skip-aot tg-rss-bot.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

