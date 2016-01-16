(ns tg-rss-bot.core
  (:require [tg-rss-bot.tgapi :as tgapi]
            [clojure.tools.logging :as log]
            [clojure.core.match :refer [match]])
  (:gen-class))

(defn updates-seq
  ([bot] (updates-seq bot 0))
  ([bot offset]
   (Thread/sleep 500)
   (let [updates (try (tgapi/get-updates bot offset)
                      (catch Exception e (log/error e "Get updates fail") []))
         new-offset (-> updates (last) (get :update_id -1) (+ 1))]
     (lazy-cat updates (updates-seq bot new-offset)))))

(defn handle-message [bot]
  (loop [updates (updates-seq bot)]
    (println (first updates))
    (when-let [message ((first updates) :message)]
      (try
        (match (tgapi/parse-cmd bot (message :text))
               ["rss" _] (prn "RSS!")
               ["sub" url] (prn "sub!" url)
               [cmd arg] (log/warnf "Unknown command: %s, args: %s"
                                    cmd arg)
               :else (log/warnf "Unable to parse command: %s"
                                (message :text)))
        (catch Exception e (log/error e ""))))
    (recur (rest updates))))

(defn pull-rss-updates [bot])

(defn -main [bot-key]
  (let [bot (tgapi/new-bot bot-key)]
    (.start (Thread. (fn [](pull-rss-updates bot))))
    (handle-message bot)))
