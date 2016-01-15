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
        result (request (str api "/getMe"))]
   (assoc result :api api)))

(defn get-updates
  ([bot] (get-updates bot 0))
  ([bot offset]
   (req bot "getUpdates" {"offset" offset})))

(defn- if-not-nil-add [k v m]
  (if v
    (assoc m k v)
    m))

(defn answer-inline-query [bot inline_query_id results &
                          {:keys [cache_time is_personal next_offset]}]
  (req bot "answerInlineQuery"
       (->> {"inline_query_id" inline_query_id
             "results" results}
            (if-not-nil-add "cache_time" cache_time)
            (if-not-nil-add "is_personal" is_personal)
            (if-not-nil-add "next_offset" next_offset))))
