(ns tg-rss-bot.tgapi
  (:require [clojure.string :as string]
            [clj-http.client :as client]
            [clojure.data.json :as json]))

(defn- request
  ([url] (request url nil))
  ([url data]
   (let [resp (if data
                (client/post url {:content-type :json
                                  :body (json/write-str data)
                                  :throw-exceptions false})
                (client/get url {:throw-exceptions false}))
         code (resp :status)]
     (if (or (and (>= code 200) (< code 300))
             (and (>= code 400) (< code 500)))
       (let [body (json/read-str (resp :body) :key-fn keyword)]
         (when-not (body :ok)
           (throw (ex-info (format "HTTP: %s, %s"
                                   code
                                   (body :description)) resp)))
         (body :result))
       (throw (ex-info (format "HTTP: %s" code) resp))))))

(defn req
  ([bot method] (req bot method nil))
  ([bot method data]
   (request (str (bot :api) "/" method) data)))

(defn new-bot [bot-key]
  (let [api (str "https://api.telegram.org/bot" bot-key)
        result (request (str api "/getMe"))
        cmd-reg (re-pattern
                  (str "^\\/(\\w+)(?:@" (result :username) ")?(?: +(.+)?)?$"))]
   (assoc result :api api :cmd-reg cmd-reg)))

(defn parse-cmd [bot text]
  (vec (rest (re-find (bot :cmd-reg) text))))

(defn get-updates
  ([bot] (get-updates bot 0))
  ([bot offset]
   (req bot "getUpdates" {"offset" offset})))

(defn- if-not-nil-add [k v m]
  (if v
    (assoc m k v)
    m))

(defn send-message [bot chat-id text &
                    {:keys [parse-mode disable-web-page-preview
                            reply-to-message-id reply-markup]}]
  (req bot "sendMessage"
       (->> {"chat_id" chat-id
             "text" text}
            (if-not-nil-add "parse_mode" parse-mode)
            (if-not-nil-add "disable_web_page_preview" disable-web-page-preview)
            (if-not-nil-add "reply_to_message_id" reply-to-message-id)
            (if-not-nil-add "reply_markup" reply-markup))))

(defn answer-inline-query [bot inline-query-id results &
                          {:keys [cache-time is-personal next-offset]}]
  (req bot "answerInlineQuery"
       (->> {"inline_query_id" inline-query-id
             "results" results}
            (if-not-nil-add "cache_time" cache-time)
            (if-not-nil-add "is_personal" is-personal)
            (if-not-nil-add "next_offset" next-offset))))
