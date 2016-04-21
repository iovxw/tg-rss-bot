(ns tg-rss-bot.core
  (:require [tg-rss-bot.tgapi :as tgapi]
            [clojure.tools.logging :as log]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as string]
            [clj-http.client :as client]
            [clojure.core.async :refer [go <!! timeout]]
            [clojure.core.match :refer [match]]
            [feedparser-clj.core :as feedparser])
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

(defn split-message [text max-len]
  (let [text-len (count text)]
    (loop [begin 0, offset 0, result []]
      (let [n (if-let [n (string/index-of text "\n" offset)] n text-len)
            line-len (- n offset)
            now-len (- n begin)] ; 当前所选定的长度
        (if (<= (- text-len begin) max-len) ; 剩下的长度小于最大长度
          (conj result (subs text begin text-len)) ; DONE
          (if (> line-len max-len) ; 单行大于最大长度，进行行内切分
            (let [split-len (- max-len (- offset begin))
                  split-point (+ offset split-len)]
              (recur split-point split-point
                     (conj result (subs text begin split-point))))
            (if (> now-len max-len)
              (recur offset (inc n) ; 这里的 (dec offset) 是为了去掉最后一个换行
                     (conj result (subs text begin (dec offset))))
              (recur begin (inc n) result))))))))

(defn send-message [bot chat-id text & opts]
  (if (<= (count text) 1024)
    (apply tgapi/send-message bot chat-id text opts)
    (let [messages (split-message text 1024)]
      (doseq [msg messages]
        (apply tgapi/send-message bot chat-id msg opts)))))

(defn parse-feed [url]
  (let [resp (client/get url {:as :stream
                              :headers {"User-Agent"
                                        (str "Mozilla/5.0 (X11; Linux x86_64) "
                                             "AppleWebKit/537.36 (KHTML, like Gecko) "
                                             "Chrome/49.0.2623.110 Safari/537.36")}})]
    (feedparser/parse-feed (resp :body))))

(defn format-title [title]
  (string/replace title #"(?:^[\s\n]*)|(?:[\s\n]*$)" ""))

(defn sub-rss [bot db url subscriber]
  (try
    (if-not (has-row db "subscribers"
                 "rss = ? AND subscriber = ?" url subscriber)
      (let [rss (parse-feed url)
            title (format-title (rss :title))]
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
  (string/escape title {\< "&lt;", \> "&gt;", \& "&amp;"}))

(defn get-rss-title [db url]
  (-> (jdbc/query db ["SELECT title FROM rss WHERE url = ?" url])
      (first)
      (get :title)))

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

(defn get-sub-list [bot db subscriber raw?]
  (let [result (jdbc/query db ["SELECT rss FROM subscribers
                                WHERE subscriber = ?" subscriber])]
    (if-not (= (count result) 0)
      (if raw?
        (send-message bot subscriber
                      (reduce #(format "%s\n%s: %s" %1
                                       (get-rss-title db (%2 :rss)) (%2 :rss))
                              "订阅列表:" result)
                      :disable-web-page-preview true)
        (send-message bot subscriber
                      (reduce #(format "%s\n<a href=\"%s\">%s</a>" %1 (%2 :rss)
                                       (escape-title (get-rss-title db (%2 :rss))))
                              "订阅列表:" result)
                      :parse-mode "HTML"
                      :disable-web-page-preview true))
      (tgapi/send-message bot subscriber "订阅列表为空"))))

(defn handle-update [bot db update]
  (prn update)
  (when-let [message (update :message)]
    (when-let [text (message :text)]
      (match (tgapi/parse-cmd bot text)
             ["rss" raw] (get-sub-list bot db (get-in message [:chat :id])
                                       (if (= raw "raw") true false))
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
  (reduce #(format "%s\n<a href=\"%s\">%s</a>" %1 (%2 :link) (escape-title (%2 :title)))
          (format "<b>%s</b>" (escape-title title)) updates))

(defn merge-hash-list [src dst]
  (let [max-len (* (count src) 2)
        result (vec (distinct (concat src dst)))]
    (if (> (count result) max-len)
      (subvec result 0 max-len)
      result)))

(defn pull-rss-updates [bot db]
  (doseq [row (get-all-rss db)]
    (go (try
          (let [url (row :url)
                hash-list (string/split (row :hash_list) #" ")
                rss (parse-feed url)
                new-hash-list (gen-hash-list rss)
                title (format-title (rss :title))
                updates (filter-updates hash-list new-hash-list (rss :entries))]
            (when (not= (count updates) 0)
              (jdbc/update! db :rss {:title title
                                     :hash_list (string/join " " (merge-hash-list new-hash-list hash-list))}
                            ["url = ?" url])
              (let [message (make-rss-update-msg title updates)]
                (doseq [subscriber (get-subscribers db url)]
                  (send-message bot subscriber message
                                :parse-mode "HTML"
                                :disable-web-page-preview true)))))
          (catch Exception e
            (log/warnf "Pull RSS updates fail: %s\n%s" (row :url) (.getMessage e))))))
  (<!! (timeout 300000)) ; 5min
  (recur bot db))

(defn -main [bot-key]
  (init-db db)
  (let [bot (tgapi/new-bot bot-key)]
    (go (pull-rss-updates bot db))
    (loop [updates (updates-seq bot)]
      (go (handle-update bot db (first updates)))
      (recur (rest updates)))))
