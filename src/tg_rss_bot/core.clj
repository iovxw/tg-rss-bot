(ns tg-rss-bot.core
  (:require [tg-rss-bot.tgapi :as tgapi]
            [clojure.tools.logging :as log]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as string]
            [clj-http.client :as client]
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
                      subscriber VARCHAR)"]))

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

(defn has-row? [db table query & value]
  (let [query (format "SELECT COUNT(*) FROM %s WHERE %s"
                      table query)
        result (jdbc/query db (cons query value))]
    ;; result like ({:count(*) 1})
    (not (zero? ((keyword "count(*)") (first result))))))

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

(def status-code
  {100 "Continue"
   101 "Switching Protocols"
   102 "Processing"
   200 "OK"
   201 "Created"
   202 "Accepted"
   203 "Non-Authoritative Information"
   204 "No Content"
   205 "Reset Content"
   206 "Partial Content"
   207 "Multi-Status"
   208 "Already Reported"
   226 "IM Used"
   300 "Multiple Choices"
   301 "Moved Permanently"
   302 "Found"
   303 "See Other"
   304 "Not Modified"
   305 "Use Proxy"
   306 "Switch Proxy"
   307 "Temporary Redirect"
   308 "Permanent Redirect"
   400 "Bad Request"
   401 "Unauthorized"
   402 "Payment Required"
   403 "Forbidden"
   404 "Not Found"
   405 "Method Not Allowed"
   406 "Not Acceptable"
   407 "Proxy Authentication Required"
   408 "Request Timeout"
   409 "Conflict"
   410 "Gone"
   411 "Length Required"
   412 "Precondition Failed"
   413 "Payload Too Large"
   414 "URI Too Long"
   415 "Unsupported Media Type"
   416 "Range Not Satisfiable"
   417 "Expectation Failed"
   418 "I'm a teapot"
   421 "Misdirected Request"
   422 "Unprocessable Entity"
   423 "Locked"
   424 "Failed Dependency"
   426 "Upgrade Required"
   428 "Precondition Required"
   429 "Too Many Requests"
   431 "Request Header Fields Too Large"
   451 "Unavailable For Legal Reasons"
   500 "Internal Server Error"
   501 "Not Implemented"
   502 "Bad Gateway"
   503 "Service Unavailable"
   504 "Gateway Timeout"
   505 "HTTP Version Not Supported"
   506 "Variant Also Negotiates"
   507 "Insufficient Storage"
   508 "Loop Detected"
   510 "Not Extended"
   511 "Network Authentication Required"
   ; nginx
   444 "No Response"
   495 "SSL Certificate Error"
   496 "SSL Certificate Required"
   497 "HTTP Request Sent to HTTPS Port"
   499 "Client Closed Request"
   ; CloudFlare
   520 "Unknown Error"
   521 "Web Server Is Down"
   522 "Connection Timed Out"
   523 "Origin Is Unreachable"
   524 "A Timeout Occurred"
   525 "SSL Handshake Failed"
   526 "Invalid SSL Certificate"})

(defn parse-feed [url]
  (try
    ;; fix ParsingFeedException
    ;; https://github.com/rometools/rome/issues/222
    (let [resp (client/get url {:as :stream
                                :headers {"User-Agent"
                                          (str "Mozilla/5.0 (X11; Linux x86_64) "
                                               "AppleWebKit/537.36 (KHTML, like Gecko) "
                                               "Chrome/52.0.2743.82 Safari/537.36")}})]
      (feedparser/parse-feed (resp :body)))
    (catch java.net.UnknownHostException _
      (throw (ex-info "未知服务器地址" {:type :rss-exception})))
    (catch clojure.lang.ExceptionInfo e
      (throw (ex-info (let [code (:status (ex-data e))]
                        (if code
                          (format "HTTP %s: %s" code (get status-code code))
                          (.getMessage e)))
                      {:type :rss-exception})))
    (catch Exception e
      (throw (ex-info (.getMessage e) {:type :rss-exception})))))

(defn format-title [title]
  (when title
    (string/replace title #"(?:^[\s\n]*)|(?:[\s\n]*$)" "")))

(defn escape-title [title]
  (when title
    (string/escape title {\< "&lt;", \> "&gt;", \& "&amp;"})))

(defn entry-hash [entry]
  (hash (str (:link entry) (:title entry))))

(defn gen-rss-hash-list [rss]
  (string/join " " (map entry-hash (:entries rss))))

(defn sub-rss [bot db chat-id url subscriber]
  (if-not (has-row? db "subscribers"
                    "rss = ? AND subscriber = ?" url subscriber)
    (let [msg (tgapi/send-message bot chat-id "拉取 RSS 信息中，请稍候")
          msg-id (:message_id msg)
          rss (try (parse-feed url)
                   (catch Exception e
                     (tgapi/edit-message-text bot chat-id msg-id
                                              (format "订阅失败: %s" (.getMessage e)))
                     (log/warnf "sub-rss: %s, %s" url (.getMessage e))))
          title (format-title (:title rss))]
      (when rss
        (jdbc/insert! db :subscribers
                      {:rss url
                       :subscriber subscriber})
        (tgapi/edit-message-text bot chat-id msg-id
                                 (format "《<a href=\"%s\">%s</a>》订阅成功"
                                         url (escape-title title))
                                 :parse-mode "HTML"
                                 :disable-web-page-preview true)
        ;; 检查是否为第一次订阅
        (when-not (has-row? db "rss" "url = ?" url)
          (jdbc/insert! db :rss {:url url :title title
                                 :hash_list (gen-rss-hash-list rss)
                                 :err_count 0}))))
    (tgapi/send-message bot chat-id "订阅失败，已经订阅过的 RSS")))

(defn get-rss-title [db url]
  (-> (jdbc/query db ["SELECT title FROM rss WHERE url = ?" url])
      (first)
      (:title)))

(defn unsub-rss [bot db chat-id url subscriber]
  (let [result (jdbc/execute! db ["DELETE FROM subscribers
                                   WHERE rss = ? AND subscriber = ?"
                                  url subscriber])]
    (if (>= (first result) 1)
      (let [title (get-rss-title db url)]
        (when-not (has-row? db "subscribers" "rss = ?" url)
          ;; 最后一个订阅者退订，删除这个 RSS
          (jdbc/delete! db :rss ["url = ?" url]))
        (tgapi/send-message bot chat-id (format "《<a href=\"%s\">%s</a>》退订成功"
                                                url (escape-title title))
                            :parse-mode "HTML"
                            :disable-web-page-preview true))
      (tgapi/send-message bot chat-id "退订失败，没有订阅过的 RSS"))))

(defn get-sub-list [bot db chat-id subscriber raw?]
  (let [result (jdbc/query db ["SELECT rss FROM subscribers
                                WHERE subscriber = ?" subscriber])]
    (if-not (= (count result) 0)
      (if raw?
        (send-message bot chat-id
                      (reduce #(format "%s\n%s: %s" %1
                                       (get-rss-title db (:rss %2)) (:rss %2))
                              "订阅列表:" result)
                      :disable-web-page-preview true)
        (send-message bot chat-id
                      (reduce #(format "%s\n<a href=\"%s\">%s</a>" %1 (:rss %2)
                                       (escape-title (get-rss-title db (:rss %2))))
                              "订阅列表:" result)
                      :parse-mode "HTML"
                      :disable-web-page-preview true))
      (tgapi/send-message bot chat-id "订阅列表为空"))))

(defmacro match-args [args & body]
  `(match (when ~args (string/split ~args #" "))
     ~@body))

(defn is-channel? [bot channel-id]
  (try
    (-> (tgapi/get-chat bot channel-id)
        :type
        (= "channel"))
    (catch Exception e
      (if (-> (ex-data e)
              :status
              (= 400))
        false ; Bad Request: chat not found
        (throw e)))))

(defn is-admin? [bot chat-id user-id]
  (try
    (some #(= (get-in % [:user :id]) user-id)
          (tgapi/get-chat-admins bot chat-id))
    (catch Exception e
      (if (-> (ex-data e)
              :status
              (= 400))
        false ; Bad Request: Channel members are unavailable
        (throw e)))))

(defn user-and-bot-is-channel-admin? [bot chat-id channel-id user-id]
  (let [msg (tgapi/send-message bot chat-id "正在验证 Channel")
        msg-id (:message_id msg)]
    (if (is-channel? bot channel-id)
      (if (is-admin? bot channel-id (:id bot))
        (if (is-admin? bot channel-id user-id)
          (do (tgapi/edit-message-text bot chat-id msg-id "Channel 验证完成")
              true)
          (do (tgapi/edit-message-text bot chat-id msg-id "该命令只能由 Channel 管理员使用")
              false))
        (do (tgapi/edit-message-text bot chat-id msg-id "请先将本 Bot 设为该 Channel 管理员")
            false))
      (do (tgapi/edit-message-text bot chat-id msg-id "目标需为 Channel")
          false))))

(defn handle-update [bot db update]
  (try
    (when-let [message (:message update)]
      (when-let [text (:text message)]
        (let [[cmd args] (tgapi/parse-cmd bot text)]
          (case cmd
            "start" (tgapi/send-message bot (get-in message [:chat :id])
                                        (str "命令列表：\n"
                                             "/rss - 显示当前订阅的 RSS 列表，可以加 raw 参数显示原始链接\n"
                                             "/sub - 命令后加要订阅的 RSS 链接，订阅一条 RSS\n"
                                             "/unsub - 命令后加要退订的 RSS 链接，退订一条 RSS\n"
                                             "本项目源码：\n"
                                             "https://github.com/iovxw/tg-rss-bot"))
            "rss" (match-args args
                    nil (get-sub-list bot db (get-in message [:chat :id]) (get-in message [:chat :id]) false)
                    ["raw"] (get-sub-list bot db (get-in message [:chat :id]) (get-in message [:chat :id]) true)
                    [channel-id] (when (user-and-bot-is-channel-admin?
                                        bot (get-in message [:chat :id])
                                        channel-id (get-in message [:from :id]))
                                   (get-sub-list bot db (get-in message [:chat :id]) channel-id false))
                    [channel-id "raw"] (when (user-and-bot-is-channel-admin?
                                              bot (get-in message [:chat :id])
                                              channel-id (get-in message [:from :id]))
                                         (get-sub-list bot db (get-in message [:chat :id]) channel-id true))
                    :else (tgapi/send-message bot (get-in message [:chat :id])
                                              "使用方法： /rss <Channel ID> <raw>"))
            "sub" (match-args args
                    [url] (sub-rss bot db (get-in message [:chat :id]) url (get-in message [:chat :id]))
                    [channel-id url] (when (user-and-bot-is-channel-admin?
                                            bot (get-in message [:chat :id])
                                            channel-id (get-in message [:from :id]))
                                       (sub-rss bot db (get-in message [:chat :id]) url channel-id))
                    :else (tgapi/send-message bot (get-in message [:chat :id])
                                              "使用方法： /sub <Channel ID> [RSS URL]"))
            "unsub" (match-args args
                      [url] (unsub-rss bot db (get-in message [:chat :id]) url (get-in message [:chat :id]))
                      [channel-id url] (when (user-and-bot-is-channel-admin?
                                              bot (get-in message [:chat :id])
                                              channel-id (get-in message [:from :id]))
                                         (unsub-rss bot db (get-in message [:chat :id]) url channel-id))
                      :else (tgapi/send-message bot (get-in message [:chat :id])
                                                "使用方法： /unsub <Channel ID> [RSS URL]"))
            (log/warnf "Unknown command: %s, args: %s" cmd args)))))
    (catch Exception e
      (log/error "Unexpected error" e))))

(defn get-all-rss [db]
  (jdbc/query db ["SELECT * FROM rss"]))

(defn get-subscribers [db rss]
  (let [result (jdbc/query db ["SELECT subscriber FROM subscribers
                                WHERE rss = ?" rss])]
    (map #(:subscriber %) result)))

(defn fix-relative-url [host link]
  (cond
    (string/starts-with? link "//")
    (str "http:" link)

    (string/starts-with? link "/")
    (str host link)

    (string/starts-with? link "./")
    (str host (string/replace-first link "." ""))

    :else link))

(defn get-host [url]
  (subs url 0 (string/index-of url "/" 8)))

(defn make-rss-update-msg [url title updates]
  (reduce #(format "%s\n<a href=\"%s\">%s</a>"
                   %1
                   (fix-relative-url (get-host url) (or (:link %2) (:uri %2) ""))
                   (-> (:title %2)
                       (format-title)
                       (escape-title)))
          (format "<b>%s</b>" (escape-title title)) updates))

(defn filter-updates [hash-list entries]
  (let [hash-list (apply hash-set hash-list)] ; convert to hash-set for (contains?)
    (remove #(contains? hash-list (entry-hash %)) entries)))

(defn update-rss-hash-list [old-hash-list new-entries rss]
  (let [hash-list (map entry-hash new-entries)
        max-size (* (count (:entries rss)) 2)
        result (concat hash-list old-hash-list)
        result (if (> (count result) max-size)
                 (take max-size result)
                 result)]
    (string/join " " result)))

(defn parse-int [s]
  (try (Integer. s)
       (catch Exception _
         nil)))

(defn pull-rss-updates [bot db]
  (future
    (loop []
      (doseq [row (get-all-rss db)]
        (future
          (try
            (let [url (:url row)
                  hash-list (keep parse-int (string/split (:hash_list row) #" "))
                  rss (parse-feed url)
                  title (format-title (:title rss))
                  updates (filter-updates hash-list (:entries rss))]
              (when (not= (count updates) 0)
                (jdbc/update! db :rss {:title title
                                       :hash_list (update-rss-hash-list hash-list updates rss)
                                       :err_count 0}
                              ["url = ?" url])
                (let [message (make-rss-update-msg url title updates)]
                  (doseq [subscriber (get-subscribers db url)]
                    (try
                      (send-message bot subscriber message
                                    :parse-mode "HTML"
                                    :disable-web-page-preview true)
                      (catch Exception e
                        (when (-> e ex-data :status (= 400))
                          (unsub-rss bot db subscriber url subscriber)) ; 强制退订
                        (throw e)))))))
            (catch Exception e
              (let [msg (.getMessage e)
                    url (:url row)
                    title (:title row)
                    err-count (inc (:err_count row))]
                (log/errorf e "Pull RSS updates fail: %s" url)
                (when (= (:type (ex-data e)) :rss-exception)
                  (if (< err-count 1440)
                    (jdbc/update! db :rss {:err_count err-count} ["url = ?" url])
                    (do (doseq [subscriber (get-subscribers db url)]
                          (try
                            (send-message bot subscriber
                                          (format "《<a href=\"%s\">%s</a>》已经连续五天拉取出错（%s），可能已经关闭，请取消订阅"
                                                  url (escape-title title) msg)
                                          :parse-mode "HTML"
                                          :disable-web-page-preview true)
                            (catch Exception e
                              (when (-> e ex-data :status (= 400))
                                (unsub-rss bot db subscriber url subscriber)) ; 强制退订
                              (throw e))))
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
