(ns tg-rss-bot.core
  (:require [tg-rss-bot.tgapi :as tgapi]
            [clojure.tools.logging :as log]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as string]
            [clojure.core.async :refer [go]]
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
                      (catch Exception e
                        (log/warnf "Get updates fail: %s" (.getMessage e)) []))
         new-offset (if (not= (count updates) 0)
                      (-> updates (last) (get :update_id) (+ 1))
                      offset)] ; updates 数量为 0，可能获取失败，使用旧的 offset
     (lazy-cat updates (updates-seq bot new-offset)))))

(defn bytes-hash-to-str [hash-bytes]
  (.toString (java.math.BigInteger. 1 hash-bytes) 16))

(defn get-hash [data]
  (->> (.getBytes data)
       (.digest (java.security.MessageDigest/getInstance "SHA-1"))
       (bytes-hash-to-str)))

(defn gen-hash-list [rss]
  (map #(get-hash (str (get % :link) (get % :title))) (get rss :entries)))

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
            title (rss :title)]
        (jdbc/insert! db :subscribers
                      {:rss url
                       :subscriber subscriber})
        (tgapi/send-message bot subscriber
                            (format "《%s》订阅成功" title))
        ; 检查是否为第一次订阅
        (when-not (has-row db "rss" "url = ?" url)
          (jdbc/insert! db :rss {:url url :title title
                                 :hash_list (string/join " " (gen-hash-list rss))})))
      (tgapi/send-message bot subscriber "订阅失败，已经订阅过的 RSS"))
    (catch Exception e
      (tgapi/send-message bot subscriber
                          (format "订阅失败: %s" (.getMessage e)))
      (log/warnf "sub-rss: %s, %s" url (.getMessage e)))))

(defn escape-title [title]
  (string/replace title #"\[|\]|\(|\)"
                  {"[" "［"
                   "]" "］"
                   "(" "（"
                   ")" "）"}))

(defn get-rss-title [db url]
  (-> (jdbc/query db ["SELECT title FROM rss WHERE url = ?" url])
      (first)
      (get :title)
      (escape-title)))

(defn unsub-rss [bot db url subscriber]
  (let [result (jdbc/execute! db ["DELETE FROM subscribers
                                   WHERE rss = ? AND subscriber = ?"
                                  url subscriber])]
    (if (>= (first result) 1)
      (let [title (get-rss-title db url)]
        (tgapi/send-message bot subscriber (format "《%s》退订成功" title))
        (when-not (has-row db "subscribers" "rss = ?" url)
          ; 最后一个订阅者退订，删除这个 RSS
          (jdbc/delete! db :rss ["url = ?" url])))
      (tgapi/send-message bot subscriber "退订失败，没有订阅过的 RSS"))))

(defn get-sub-list [bot db subscriber]
  (let [result (jdbc/query db ["SELECT rss FROM subscribers
                                WHERE subscriber = ?" subscriber])]
    (if-not (= (count result) 0)
      (let [message (reduce #(format "%s\n[%s](%s)"
                                     %1 (get-rss-title db (%2 :rss)) (%2 :rss))
                            "订阅列表:" result)]
        (tgapi/send-message bot subscriber message
                            :parse-mode "Markdown"
                            :disable-web-page-preview true))
      (tgapi/send-message bot subscriber "订阅列表为空"))))

(defn handle-update [bot db update]
  (prn update)
  (when-let [message (update :message)]
    (when-let [text (message :text)]
      (match (tgapi/parse-cmd bot text)
             ["rss" _] (get-sub-list bot db (get-in message [:chat :id]))
             ["sub" url] (sub-rss bot db url (get-in message [:chat :id]))
             ["unsub" url] (unsub-rss bot db url (get-in message [:chat :id]))
             [cmd arg] (log/warnf "Unknown command: %s, args: %s" cmd arg)
             :else (log/warnf "Unable to parse command: %s" (message :text))))))

(defn get-all-rss [db]
  (jdbc/query db ["SELECT * FROM rss"]))

(defn get-subscribers [db rss]
  (let [result (jdbc/query db ["SELECT subscriber FROM subscribers
                                WHERE rss = ?" rss])]
    (map #(get % :subscriber) result)))

(defn filter-updates [hash-list new-hash-list entries]
  (for [[nh entry] (zipmap new-hash-list entries)
        :when (not (some #(= % nh) hash-list))]
    entry))

(defn make-rss-update-msg [title updates]
  (reduce #(format "%s\n[%s](%s)" %1 (escape-title (%2 :title)) (%2 :link))
          (format "*%s*" title) updates))

(defn pull-rss-updates [bot db]
  (doseq [row (get-all-rss db)]
    (try
      (let [url (row :url)
            hash-list (string/split (row :hash_list) #" ")
            rss (parse-feed url)
            new-hash-list (gen-hash-list rss)
            title (rss :title)
            updates (filter-updates hash-list new-hash-list (rss :entries))]
        (when (not= (count updates) 0)
          (jdbc/update! db :rss {:title title
                                 :hash_list (string/join " " new-hash-list)}
                        ["url = ?" url])
          (let [message (make-rss-update-msg title updates)]
            (doseq [subscriber (get-subscribers db url)]
              (tgapi/send-message bot subscriber message
                                  :parse-mode "Markdown"
                                  :disable-web-page-preview true)))))
      (catch Exception e
        (log/error e (format "Pull RSS updates fail: %s" (row :url))))))
  (Thread/sleep 300000) ; 5min
  (recur bot db))

(defn -main [bot-key]
  (init-db db)
  (let [bot (tgapi/new-bot bot-key)]
    (go (pull-rss-updates bot db))
    (loop [updates (updates-seq bot)]
      (go (handle-update bot db (first updates)))
      (recur (rest updates)))))
