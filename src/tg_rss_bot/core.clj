(ns tg-rss-bot.core
  (:require [tg-rss-bot.tgapi :as tgapi]
            [clojure.tools.logging :as log]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as string]
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
                      hash_list VARCHAR,
                      err_count INTEGER)"])
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
                      (-> updates last :update_id inc)
                      offset)] ; updates 数量为 0，可能获取失败，使用旧的 offset
     (lazy-cat updates (updates-seq bot new-offset)))))

(defn bytes-hash-to-str [hash-bytes]
  (.toString (java.math.BigInteger. 1 hash-bytes) 16))

(defn get-hash [data]
  (->> (.getBytes data)
       (.digest (java.security.MessageDigest/getInstance "SHA-1"))
       (bytes-hash-to-str)))

(defn gen-hash-list [rss]
  (map #(get-hash (str (:link %) (:title %))) (:entries rss)))

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
  (try
    (feedparser/parse-feed url :user-agent (str "Mozilla/5.0 (X11; Linux x86_64) "
                                                "AppleWebKit/537.36 (KHTML, like Gecko) "
                                                "Chrome/52.0.2743.82 Safari/537.36"))
    (catch java.io.FileNotFoundException _
      (throw (ex-info "页面不存在" {:type :rss-exception})))
    (catch java.net.UnknownHostException _
      (throw (ex-info "未知服务器地址" {:type :rss-exception})))
    (catch Exception e
      (throw (ex-info (.getMessage e) {:type :rss-exception})))))

(defn format-title [title]
  (string/replace title #"(?:^[\s\n]*)|(?:[\s\n]*$)" ""))

(defn escape-title [title]
  (string/escape title {\< "&lt;", \> "&gt;", \& "&amp;"}))

(defn sub-rss [bot db url subscriber]
  (try
    (if-not (has-row db "subscribers"
                 "rss = ? AND subscriber = ?" url subscriber)
      (let [rss (parse-feed url)
            title (format-title (:title rss))]
        (jdbc/insert! db :subscribers
                      {:rss url
                       :subscriber subscriber})
        (tgapi/send-message bot subscriber
                            (format "《<a href=\"%s\">%s</a>》订阅成功"
                                    url (escape-title title))
                            :parse-mode "HTML"
                            :disable-web-page-preview true)
        ; 检查是否为第一次订阅
        (when-not (has-row db "rss" "url = ?" url)
          (jdbc/insert! db :rss {:url url :title title
                                 :hash_list (string/join " " (gen-hash-list rss))
                                 :err_count 0})))
      (tgapi/send-message bot subscriber "订阅失败，已经订阅过的 RSS"))
    (catch Exception e
      (tgapi/send-message bot subscriber
                          (format "订阅失败: %s" (.getMessage e)))
      (log/warnf "sub-rss: %s, %s" url (.getMessage e)))))

(defn get-rss-title [db url]
  (-> (jdbc/query db ["SELECT title FROM rss WHERE url = ?" url])
      (first)
      (:title)))

(defn unsub-rss [bot db url subscriber]
  (let [result (jdbc/execute! db ["DELETE FROM subscribers
                                   WHERE rss = ? AND subscriber = ?"
                                  url subscriber])]
    (if (>= (first result) 1)
      (let [title (get-rss-title db url)]
        (tgapi/send-message bot subscriber (format "《<a href=\"%s\">%s</a>》退订成功"
                                                   url (escape-title title))
                            :parse-mode "HTML"
                            :disable-web-page-preview true)
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
                                       (get-rss-title db (:rss %2)) (:rss %2))
                              "订阅列表:" result)
                      :disable-web-page-preview true)
        (send-message bot subscriber
                      (reduce #(format "%s\n<a href=\"%s\">%s</a>" %1 (:rss %2)
                                       (escape-title (get-rss-title db (:rss %2))))
                              "订阅列表:" result)
                      :parse-mode "HTML"
                      :disable-web-page-preview true))
      (tgapi/send-message bot subscriber "订阅列表为空"))))

(defn handle-update [bot db update]
  (prn update)
  (when-let [message (:message update)]
    (when-let [text (:text message)]
      (match (tgapi/parse-cmd bot text)
        ["start" _] (tgapi/send-message bot (get-in message [:chat :id])
                                        (str "命令列表：\n"
                                             "/rss - 显示当前订阅的 RSS 列表，可以加 raw 参数显示原始链接\n"
                                             "/sub - 命令后加要订阅的 RSS 链接，订阅一条 RSS\n"
                                             "/unsub - 命令后加要退订的 RSS 链接，退订一条 RSS\n"
                                             "本项目源码：\n"
                                             "https://github.com/iovxw/tg-rss-bot"))
        ["rss" raw] (get-sub-list bot db (get-in message [:chat :id])
                                  (if (= raw "raw") true false))
        ["sub" nil] (tgapi/send-message bot (get-in message [:chat :id])
                                        "RSS 不能为空, 请在命令后加入要订阅的 RSS 地址")
        ["sub" url] (sub-rss bot db url (get-in message [:chat :id]))
        ["unsub" nil] (tgapi/send-message bot (get-in message [:chat :id])
                                          "RSS 不能为空, 请在命令后加入要退订的 RSS 地址")
        ["unsub" url] (unsub-rss bot db url (get-in message [:chat :id]))
        [cmd arg] (log/warnf "Unknown command: %s, args: %s" cmd arg)
        :else (log/warnf "Unable to parse command: %s" (:text message))))))

(defn get-all-rss [db]
  (jdbc/query db ["SELECT * FROM rss"]))

(defn get-subscribers [db rss]
  (let [result (jdbc/query db ["SELECT subscriber FROM subscribers
                                WHERE rss = ?" rss])]
    (map #(:subscriber %) result)))

(defn filter-updates [hash-list new-hash-list entries]
  (for [[nh entry] (zipmap new-hash-list entries)
        :when (not (some #(= % nh) hash-list))]
    entry))

(defn make-rss-update-msg [title updates]
  (reduce #(format "%s\n<a href=\"%s\">%s</a>" %1 (:link %2) (-> (:title %2)
                                                                 (format-title)
                                                                 (escape-title)))
          (format "<b>%s</b>" (escape-title title)) updates))

(defn merge-hash-list [src dst]
  (let [max-len (* (count src) 2)
        result (vec (distinct (concat src dst)))]
    (if (> (count result) max-len)
      (subvec result 0 max-len)
      result)))

(defn pull-rss-updates [bot db]
  (future
    (loop []
      (doseq [row (get-all-rss db)]
        (future
          (try
            (let [url (:url row)
                  hash-list (string/split (:hash_list row) #" ")
                  rss (parse-feed url)
                  new-hash-list (gen-hash-list rss)
                  title (format-title (:title rss))
                  updates (filter-updates hash-list new-hash-list (:entries rss))]
              (when (not= (count updates) 0)
                (jdbc/update! db :rss {:title title
                                       :hash_list (string/join " " (merge-hash-list new-hash-list hash-list))
                                       :err_count 0}
                              ["url = ?" url])
                (let [message (make-rss-update-msg title updates)]
                  (doseq [subscriber (get-subscribers db url)]
                    (send-message bot subscriber message
                                  :parse-mode "HTML"
                                  :disable-web-page-preview true)))))
            (catch Exception e
              (let [msg (.getMessage e)
                    url (:url row)
                    title (:title row)
                    err-count (inc (:err_count row))]
                (log/warnf "Pull RSS updates fail: %s\n%s" url msg)
                (when (= (:type (ex-data e)) :rss-exception)
                  (if (< err-count 1440)
                    (jdbc/update! db :rss {:err_count err-count})
                    (do (doseq [subscriber (get-subscribers db url)]
                          (send-message bot subscriber
                                        (format "《<a href=\"%s\">%s</a>》已经连续五天拉取出错（%s），可能已经关闭，请取消订阅"
                                                url (escape-title title) msg)
                                        :parse-mode "HTML"
                                        :disable-web-page-preview true))
                        (jdbc/update! db :rss {:err_count 0}
                                      ["url = ?" url])))))))))
      (Thread/sleep 300000) ; 5min
      (recur))))

(defn -main [bot-key]
  (init-db db)
  (let [bot (tgapi/new-bot bot-key)]
    (pull-rss-updates bot db)
    (loop [updates (updates-seq bot)]
      (future (handle-update bot db (first updates)))
      (recur (rest updates)))))
