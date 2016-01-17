(ns tg-rss-bot.core
  (:require [tg-rss-bot.tgapi :as tgapi]
            [clojure.tools.logging :as log]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as string]
            [clojure.core.match :refer [match]]
            [feedparser-clj.core :refer [parse-feed]])
  (:gen-class))

(def db
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     "database.db"})

(defn init-db [db]
  (jdbc/execute! db ["CREATE TABLE IF NOT EXISTS rss (
                      url       VARCHAR,
                      title     VARCHAR,
                      hash_list VARCHAR)"])
  (jdbc/execute! db ["CREATE TABLE IF NOT EXISTS subscribers (
                      rss        VARCHAR,
                      subscriber INTEGER)"]))

(defn updates-seq
  ([bot] (updates-seq bot 0))
  ([bot offset]
   (Thread/sleep 500)
   (let [updates (try (tgapi/get-updates bot offset)
                      (catch Exception e (log/error e "Get updates fail") []))
         new-offset (-> updates (last) (get :update_id -1) (+ 1))]
     (lazy-cat updates (updates-seq bot new-offset)))))

(defn bytes-hash-to-str [hash-bytes]
  (.toString (java.math.BigInteger. 1 hash-bytes) 16))

(defn get-hash [data]
  (->> (.getBytes data)
       (.digest (java.security.MessageDigest/getInstance "SHA-1"))
       (bytes-hash-to-str)))

(defn gen-hash-list [rss]
  (->> (.entries rss)
       (map #(get-hash (.value (.description %))))
       (string/join " ")))

(defn has-row [db table query & value]
  (let [query (format "SELECT COUNT(*) FROM %s WHERE %s"
                      table query)
        result (jdbc/query db (cons query value))]
    ; result like ({:count(*) 1})
    (if (= (get (first result) (keyword "count(*)")) 0)
      false
      true)))

(defn sub-rss [bot db url subscriber]
  (try
    (if-not (has-row db "subscribers"
                 "rss = ? AND subscriber = ?" url subscriber)
      (let [rss (parse-feed url)
            title (.title rss)]
        (jdbc/insert! db :subscribers
                      {:rss url
                       :subscriber subscriber})
        (tgapi/send-message bot subscriber
                            (format "《%s》订阅成功" title))
        ; 检查是否为第一次订阅
        (when (has-row db "rss" "url = ?" url)
          (jdbc/insert! db :rss {:url url :title title
                                 :hash_list (gen-hash-list rss)})))
      (tgapi/send-message bot subscriber "订阅失败，已经订阅过的 RSS"))
    (catch Exception e
      (tgapi/send-message bot subscriber "订阅失败，请检查 URL 以及是否包含 RSS")
      (prn e)
      (log/warnf "sub-rss: %s, %s" url (.getMessage e)))))

(defn unsub-rss [bot db url subscriber]
  (let [result (jdbc/execute! db ["DELETE FROM subscribers
                                   WHERE rss = ? AND subscriber = ?"
                                  url subscriber])]
    (if (>= (first result) 1)
      (tgapi/send-message bot subscriber "退订成功")
      (tgapi/send-message bot subscriber "退订失败，没有订阅过的 RSS"))))

(defn handle-message [bot db]
  (loop [updates (updates-seq bot)]
    (println (first updates))
    (when-let [message ((first updates) :message)]
      (match (tgapi/parse-cmd bot (message :text))
             ["rss" _] (prn "RSS!")
             ["sub" url] (sub-rss bot db url
                                  (get-in message [:chat :id]))
             ["unsub" url] (unsub-rss bot db url
                                      (get-in message [:chat :id]))
             [cmd arg] (log/warnf "Unknown command: %s, args: %s"
                                  cmd arg)
             :else (log/warnf "Unable to parse command: %s"
                              (message :text))))
    (recur (rest updates))))

(defn pull-rss-updates [bot db])

(defn -main [bot-key]
  (init-db db)
  (let [bot (tgapi/new-bot bot-key)]
    (.start (Thread. (fn [](pull-rss-updates bot db))))
    (handle-message bot db)))
